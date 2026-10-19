open Conex_utils
open Conex_unix_persistency
open Conex_io

let ( let* ) = Result.bind

let realpath dir =
  (* Unix.realpath is only 4.13+ *)
  let cwd_chdir dir =
    let cwd =
      try Sys.getcwd ()
      with Sys_error _ -> Filename.get_temp_dir_name ()
    in
    Unix.chdir dir;
    cwd
  in
  try cwd_chdir (cwd_chdir dir) with Unix.Unix_error _ -> dir

let fs_provider basedir =
  let basedir = realpath basedir in
  let* () =
    if not (exists basedir) then
      mkdir basedir
    else
      Ok ()
  in
  let get path = path_to_string (basedir :: path) in
  let ensure_dir path =
    let rec mkdir base = function
      | [] -> Ok ()
      | [_] -> Ok ()
      | x::xs ->
         let path = base @ [x] in
         let str = path_to_string path in
         let* () =
           if not (exists str) then
             Conex_unix_persistency.mkdir (path_to_string path)
           else
             Ok ()
         in
         let* ft = file_type str in
         match ft with
         | Directory -> mkdir path xs
         | File -> Error (str ^ " is not a directory")
    in
    mkdir [basedir] path
  in
  let file_type path =
    let p = get path in
    file_type p
  and read path =
    let fn = get path in
    read_file fn
  and write path data =
    let* () = ensure_dir path in
    let nam = get path in
    write_replace nam data
  and read_dir path =
    let abs = get path in
    let* files = collect_dir abs in
    foldM (fun acc fn ->
        let fullfn = Filename.concat abs fn in
        let* ft = file_type fullfn in
        match ft with
        | File -> Ok ((File, fn) :: acc)
        | Directory -> Ok ((Directory, fn) :: acc))
      [] files
  and exists path =
    exists (get path)
  in
  Ok { basedir ; description = "File system provider" ; file_type ; read ; write ; read_dir ; exists }

let fs_ro_provider basedir =
  let* fs = fs_provider basedir in
  let write _ _ = Ok ()
  and description = "Read only file system provider"
  in
  Ok { fs with description ; write }

let rec run_tar : type a e t. in_channel -> (a, e, t) Tar.t -> (a, e) result =
  fun ic -> function
  | Tar.Return r -> r
  | Tar.Bind (x, f) ->
    let* v = run_tar ic x in
    run_tar ic (f v)
  | Tar.Read len ->
    let buf = Bytes.create (Int64.to_int len) in
    let n = input ic buf 0 (Bytes.length buf) in
    Ok (Bytes.sub_string buf 0 n)
  | Tar.Really_read _ | Tar.Seek _ | Tar.High _ | Tar.Write _ ->
    (* Tar_gz.in_gzipped only emits Read *)
    invalid_arg "unsupported tar operation"

let tar_gz_ro_provider filename =
  (* archives written by opam have no directory entries, directories are
     derived from the paths of the files *)
  let nodes = Hashtbl.create 1024 in
  let conflict path = Error (path_to_string path ^ " is both a file and a directory") in
  let rec add_dir path =
    match Hashtbl.find_opt nodes path with
    | Some (`Dir items) -> Ok items
    | Some (`File _) -> conflict path
    | None ->
      let* () = match List.rev path with
        | [] -> Ok ()
        | name :: rparent ->
          let* parent = add_dir (List.rev rparent) in
          parent := (Directory, name) :: !parent ;
          Ok ()
      in
      let items = ref [] in
      Hashtbl.replace nodes path (`Dir items) ;
      Ok items
  in
  let add_file name data =
    let name =
      if String.is_prefix ~prefix:"./" name then String.slice ~start:2 name else name
    in
    let* path =
      if String.is_prefix ~prefix:"/" name then Error (name ^ " is an absolute path")
      else Result.map_error (fun e -> name ^ ": " ^ e) (string_to_path name)
    in
    match List.rev path with
    | [] -> Error "empty path in archive"
    | base :: rparent ->
      let* parent = add_dir (List.rev rparent) in
      match Hashtbl.find_opt nodes path with
      | Some (`Dir _) -> conflict path
      (* opam does not resolve repeated entries consistently (the first one
         wins for opam files), reject them instead of picking one *)
      | Some (`File _) -> Error (name ^ " appears more than once")
      | None ->
        parent := (File, base) :: !parent ;
        Hashtbl.replace nodes path (`File data) ;
        Ok ()
  in
  let entry ?global:_ hdr () =
    let name = hdr.Tar.Header.file_name in
    match hdr.Tar.Header.link_indicator with
    | Tar.Header.Link.Normal ->
      Tar.bind (Tar.really_read hdr.Tar.Header.file_size) (fun data ->
          Tar.return (Result.map_error (fun m -> `Msg m) (add_file name data)))
    | Tar.Header.Link.Directory -> Tar.return (Ok ())
    | _ -> Tar.return (Error (`Msg (name ^ " is neither a file nor a directory")))
  in
  let* _root = add_dir root in
  let* () =
    match
      let ic = open_in_bin filename in
      Fun.protect ~finally:(fun () -> close_in ic) (fun () ->
          run_tar ic (Tar_gz.in_gzipped (Tar.fold entry ())))
    with
    | Ok () -> Ok ()
    | Error e ->
      Error (filename ^ ": " ^ match e with
        | `Fatal e -> Format.asprintf "%a" Tar.pp_error e
        | `Eof -> "unexpected end of archive"
        | `Gz msg | `Msg msg -> msg)
    | exception Sys_error msg -> Error msg
    | exception (Invalid_argument msg | Failure msg) -> Error (filename ^ ": " ^ msg)
  in
  let find path =
    match Hashtbl.find_opt nodes path with
    | Some node -> Ok node
    | None -> Error (path_to_string path ^ " not found in " ^ filename)
  in
  let file_type path =
    let* node = find path in
    match node with `File _ -> Ok File | `Dir _ -> Ok Directory
  and read path =
    let* node = find path in
    match node with
    | `File data -> Ok data
    | `Dir _ -> Error (path_to_string path ^ " is a directory")
  and write _ _ = Ok ()
  and read_dir path =
    let* node = find path in
    match node with
    | `Dir items -> Ok !items
    | `File _ -> Error (path_to_string path ^ " is not a directory")
  and exists path = Hashtbl.mem nodes path
  in
  Ok { basedir = filename ; description = "Read only tar.gz provider" ;
       file_type ; read ; write ; read_dir ; exists }
