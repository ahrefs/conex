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
    (match run_tar ic x with
     | Ok v -> run_tar ic (f v)
     | Error e -> Error e)
  | Tar.Read len ->
    let buf = Bytes.create (Int64.to_int len) in
    let n = input ic buf 0 (Bytes.length buf) in
    Ok (Bytes.sub_string buf 0 n)
  | Tar.Really_read len -> Ok (really_input_string ic (Int64.to_int len))
  | Tar.Seek len -> seek_in ic (pos_in ic + Int64.to_int len) ; Ok ()
  | Tar.High _ | Tar.Write _ -> invalid_arg "unsupported tar operation"

let read_tar_gz filename =
  let entry ?global:_ hdr acc =
    let name = hdr.Tar.Header.file_name in
    match hdr.Tar.Header.link_indicator with
    | Tar.Header.Link.Normal ->
      Tar.bind (Tar.really_read hdr.Tar.Header.file_size) (fun data ->
          Tar.return (Ok ((name, data) :: acc)))
    | Tar.Header.Link.Directory -> Tar.return (Ok acc)
    | _ -> Tar.return (Error (`Msg (name ^ " is neither a file nor a directory")))
  in
  match
    let ic = open_in_bin filename in
    Fun.protect ~finally:(fun () -> close_in ic) (fun () ->
        run_tar ic (Tar_gz.in_gzipped (Tar.fold entry [])))
  with
  | Ok entries -> Ok (List.rev entries)
  | Error (`Fatal e) -> Error (Format.asprintf "%s: %a" filename Tar.pp_error e)
  | Error `Eof -> Error (filename ^ ": unexpected end of archive")
  | Error (`Gz msg) -> Error (filename ^ ": " ^ msg)
  | Error (`Msg msg) -> Error (filename ^ ": " ^ msg)
  | exception Sys_error msg -> Error msg
  | exception End_of_file -> Error (filename ^ ": unexpected end of file")

let tar_gz_ro_provider filename =
  let* entries = read_tar_gz filename in
  (* archives written by opam have no directory entries, directories are
     derived from the paths of the files *)
  let files = Hashtbl.create 1024 and dirs = Hashtbl.create 1024 in
  let add_item dir item = Hashtbl.replace dirs dir (item :: Hashtbl.find dirs dir) in
  let rec add_dir path =
    if Hashtbl.mem files path then
      Error (path_to_string path ^ " is both a file and a directory")
    else if Hashtbl.mem dirs path then
      Ok ()
    else begin
      Hashtbl.replace dirs path [] ;
      match List.rev path with
      | [] -> Ok ()
      | name :: rparent ->
        let parent = List.rev rparent in
        let* () = add_dir parent in
        add_item parent (Directory, name) ;
        Ok ()
    end
  in
  let add_file (name, data) =
    let name =
      if String.is_prefix ~prefix:"./" name then String.slice ~start:2 name else name
    in
    let* path =
      if String.is_prefix ~prefix:"/" name then Error (name ^ " is an absolute path")
      else string_to_path name
    in
    match List.rev path with
    | [] -> Error "empty path in archive"
    | base :: rparent ->
      let* () = add_dir (List.rev rparent) in
      if Hashtbl.mem dirs path then
        Error (name ^ " is both a file and a directory")
      else begin
        if not (Hashtbl.mem files path) then add_item (List.rev rparent) (File, base) ;
        Hashtbl.replace files path data ;
        Ok ()
      end
  in
  let* () = add_dir root in
  let* () = foldM (fun () entry -> add_file entry) () entries in
  let not_found path = Error (path_to_string path ^ " not found in " ^ filename) in
  let file_type path =
    if Hashtbl.mem files path then Ok File
    else if Hashtbl.mem dirs path then Ok Directory
    else not_found path
  and read path =
    match Hashtbl.find_opt files path with
    | Some data -> Ok data
    | None -> not_found path
  and write _ _ = Ok ()
  and read_dir path =
    match Hashtbl.find_opt dirs path with
    | Some items -> Ok items
    | None -> not_found path
  and exists path = Hashtbl.mem files path || Hashtbl.mem dirs path
  in
  Ok { basedir = filename ; description = "Read only tar.gz provider" ;
       file_type ; read ; write ; read_dir ; exists }
