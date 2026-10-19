open Conex_utils

open Common

type entry =
  | F of string * string
  | D of string
  | L of string * string

let rec run_out : type a e t. Buffer.t -> (a, e, t) Tar.t -> a = fun buf -> function
  | Tar.Write str -> Buffer.add_string buf str
  | Tar.Return (Ok x) -> x
  | Tar.Return (Error _) -> Alcotest.fail "error while writing archive"
  | Tar.Bind (x, f) -> run_out buf (f (run_out buf x))
  | Tar.Read _ | Tar.Really_read _ | Tar.Seek _ | Tar.High _ -> assert false

let stream l =
  let l = ref l in
  fun () -> match !l with
    | [] -> Tar.return (Ok None)
    | x :: xs -> l := xs ; Tar.return (Ok (Some x))

let tar_gz entries =
  let entry e =
    let make ?link_indicator ?link_name name data =
      None, Tar.Header.make ?link_indicator ?link_name name (Int64.of_int (String.length data)),
      stream [ data ]
    in
    match e with
    | F (name, data) -> make name data
    | D name -> make ~link_indicator:Tar.Header.Link.Directory name ""
    | L (name, target) -> make ~link_indicator:Tar.Header.Link.Symbolic ~link_name:target name ""
  in
  let buf = Buffer.create 1024 in
  run_out buf (Tar_gz.out_gzipped ~level:4 ~mtime:0l Gz.Unix
                 (Tar.out ~level:Tar.Header.Ustar (stream (List.map entry entries)))) ;
  Buffer.contents buf

let with_archive data f =
  let filename = Filename.temp_file "conex" ".tar.gz" in
  let oc = open_out_bin filename in
  output_string oc data ;
  close_out oc ;
  Fun.protect ~finally:(fun () -> Sys.remove filename) (fun () ->
      f (Conex_unix_provider.tar_gz_ro_provider filename))

let items = Alcotest.slist Test_provider.it compare

let check_layout entries =
  with_archive (tar_gz entries) (fun p ->
      let p = match p with Ok p -> p | Error e -> Alcotest.fail e in
      Alcotest.check (result items str_err) __LOC__
        (Ok [ File, "repo" ; Directory, "packages" ]) (p.Conex_io.read_dir []) ;
      Alcotest.check (result items str_err) __LOC__
        (Ok [ Directory, "foo" ; Directory, "bar" ]) (p.read_dir [ "packages" ]) ;
      Alcotest.check (result items str_err) __LOC__
        (Ok [ Directory, "foo.1" ; Directory, "foo.2" ]) (p.read_dir [ "packages" ; "foo" ]) ;
      Alcotest.check (result Test_provider.ft str_err) __LOC__
        (Ok Directory) (p.file_type [ "packages" ; "bar" ]) ;
      Alcotest.check (result Alcotest.string str_err) __LOC__
        (Ok "foo 1") (p.read [ "packages" ; "foo" ; "foo.1" ; "opam" ]) ;
      Alcotest.check (result Alcotest.string str_err) __LOC__
        (Error "") (p.read [ "packages" ; "baz" ]) ;
      Alcotest.check (result Alcotest.string str_err) __LOC__ (Error "") (p.read [ "packages" ]) ;
      Alcotest.check (result items str_err) __LOC__ (Error "") (p.read_dir [ "repo" ]) ;
      Alcotest.check Alcotest.bool __LOC__ true (p.exists [ "packages" ; "foo" ]) ;
      Alcotest.check Alcotest.bool __LOC__ false (p.exists [ "packages" ; "baz" ]))

(* as rewritten by opam after an incremental update: no directory entries *)
let files_only () =
  check_layout [
    F ("repo", "repo") ;
    F ("packages/foo/foo.1/opam", "foo 1") ;
    F ("packages/foo/foo.2/opam", "foo 2") ;
    F ("packages/bar/bar.1/opam", "bar 1") ;
  ]

(* as created by GNU tar: directory entries, possibly with a ./ prefix *)
let with_dirs () =
  check_layout [
    D "./" ;
    F ("./repo", "repo") ;
    D "./packages/" ;
    D "./packages/foo/" ;
    F ("./packages/foo/foo.1/opam", "foo 1") ;
    F ("./packages/foo/foo.2/opam", "foo 2") ;
    D "packages/bar/" ;
    F ("packages/bar/bar.1/opam", "bar 1") ;
  ]

let rejected () =
  let valid = tar_gz [ F ("repo", "repo") ] in
  List.iter (fun (name, data) ->
      with_archive data (fun p -> Alcotest.check Alcotest.bool name true (Result.is_error p)))
    [ "parent directory", tar_gz [ F ("packages/../../etc/passwd", "") ] ;
      "absolute path", tar_gz [ F ("/etc/passwd", "") ] ;
      "symbolic link", tar_gz [ L ("packages/foo", "/etc") ] ;
      "file and directory", tar_gz [ F ("packages/foo", "") ; F ("packages/foo/opam", "") ] ;
      "directory and file", tar_gz [ F ("packages/foo/opam", "") ; F ("packages/foo", "") ] ;
      "repeated entry", tar_gz [ F ("./packages/foo/opam", "evil") ; F ("packages/foo/opam", "good") ] ;
      "not gzip", "not a gzip file" ;
      "truncated", Stdlib.String.sub valid 0 (String.length valid / 2) ] ;
  Alcotest.check Alcotest.bool "missing" true
    (Result.is_error (Conex_unix_provider.tar_gz_ro_provider "/nonexistent.tar.gz"))

let tests = [
  "files only", `Quick, files_only ;
  "with directory entries", `Quick, with_dirs ;
  "rejected", `Quick, rejected ;
]
