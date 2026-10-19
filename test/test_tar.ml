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

let tar_gz entries =
  let entries = ref entries in
  let next () =
    match !entries with
    | [] -> Tar.return (Ok None)
    | e :: es ->
      entries := es ;
      let hdr, data = match e with
        | F (name, data) -> Tar.Header.make name (Int64.of_int (String.length data)), data
        | D name -> Tar.Header.make ~link_indicator:Tar.Header.Link.Directory name 0L, ""
        | L (name, target) ->
          Tar.Header.make ~link_indicator:Tar.Header.Link.Symbolic ~link_name:target name 0L, ""
      in
      let sent = ref false in
      let content () =
        if !sent then Tar.return (Ok None) else (sent := true ; Tar.return (Ok (Some data)))
      in
      Tar.return (Ok (Some (Some Tar.Header.Ustar, hdr, content)))
  in
  let buf = Buffer.create 1024 in
  run_out buf (Tar_gz.out_gzipped ~level:4 ~mtime:0l Gz.Unix (Tar.out ~level:Tar.Header.Ustar next)) ;
  Buffer.contents buf

let with_archive data f =
  let filename = Filename.temp_file "conex" ".tar.gz" in
  let oc = open_out_bin filename in
  output_string oc data ;
  close_out oc ;
  Fun.protect ~finally:(fun () -> Sys.remove filename) (fun () ->
      f (Conex_unix_provider.tar_gz_ro_provider filename))

let items =
  let module M = struct
    type t = item list
    let pp ppf l =
      List.iter (function
          | File, f -> Format.fprintf ppf "file %s;" f
          | Directory, d -> Format.fprintf ppf "directory %s;" d) l
    let equal a b = List.sort compare a = List.sort compare b
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let provider = function
  | Ok p -> p
  | Error e -> Alcotest.fail e

let check_layout p =
  Alcotest.check (result Test_provider.ft str_err) __LOC__ (Ok Directory) (p.Conex_io.file_type []) ;
  Alcotest.check (result items str_err) __LOC__
    (Ok [ File, "repo" ; Directory, "packages" ]) (p.read_dir []) ;
  Alcotest.check (result items str_err) __LOC__
    (Ok [ Directory, "foo" ; Directory, "bar" ]) (p.read_dir [ "packages" ]) ;
  Alcotest.check (result items str_err) __LOC__
    (Ok [ Directory, "foo.1" ; Directory, "foo.2" ]) (p.read_dir [ "packages" ; "foo" ]) ;
  Alcotest.check (result items str_err) __LOC__
    (Ok [ File, "opam" ]) (p.read_dir [ "packages" ; "foo" ; "foo.2" ]) ;
  Alcotest.check (result Test_provider.ft str_err) __LOC__ (Ok Directory) (p.file_type [ "packages" ; "bar" ]) ;
  Alcotest.check (result Test_provider.ft str_err) __LOC__ (Ok File) (p.file_type [ "packages" ; "bar" ; "bar.1" ; "opam" ]) ;
  Alcotest.check (result Alcotest.string str_err) __LOC__ (Ok "foo 1") (p.read [ "packages" ; "foo" ; "foo.1" ; "opam" ]) ;
  Alcotest.check (result Alcotest.string str_err) __LOC__ (Ok "repo") (p.read [ "repo" ]) ;
  Alcotest.check Alcotest.bool __LOC__ true (p.exists [ "packages" ; "foo" ]) ;
  Alcotest.check Alcotest.bool __LOC__ false (p.exists [ "packages" ; "baz" ]) ;
  Alcotest.check (result Alcotest.string str_err) __LOC__ (Error "") (p.read [ "packages" ; "baz" ]) ;
  Alcotest.check (result Alcotest.string str_err) __LOC__ (Error "") (p.read [ "packages" ]) ;
  Alcotest.check (result Test_provider.ft str_err) __LOC__ (Error "") (p.file_type [ "packages" ; "baz" ]) ;
  Alcotest.check (result items str_err) __LOC__ (Error "") (p.read_dir [ "repo" ]) ;
  Alcotest.check (result Alcotest.unit str_err) __LOC__ (Ok ()) (p.write [ "repo" ] "changed") ;
  Alcotest.check (result Alcotest.string str_err) __LOC__ (Ok "repo") (p.read [ "repo" ])

let files = [
  F ("repo", "repo") ;
  F ("packages/foo/foo.1/opam", "foo 1") ;
  F ("packages/foo/foo.2/opam", "foo 2") ;
  F ("packages/bar/bar.1/opam", "bar 1") ;
]

(* as rewritten by opam after an incremental update: no directory entries *)
let files_only () =
  with_archive (tar_gz files) (fun p -> check_layout (provider p))

(* as created by GNU tar: directory entries, possibly with a ./ prefix *)
let with_dirs () =
  let entries = [
    D "./" ;
    F ("./repo", "repo") ;
    D "./packages/" ;
    D "./packages/foo/" ;
    D "./packages/foo/foo.1/" ;
    F ("./packages/foo/foo.1/opam", "foo 1") ;
    D "./packages/foo/foo.2/" ;
    F ("./packages/foo/foo.2/opam", "foo 2") ;
    D "packages/bar/" ;
    D "packages/bar/bar.1/" ;
    F ("packages/bar/bar.1/opam", "bar 1") ;
  ] in
  with_archive (tar_gz entries) (fun p -> check_layout (provider p))

let long_name () =
  let name = Stdlib.String.make 80 'a' in
  let path = String.concat "/" [ "packages" ; name ; name ^ ".1.0" ; "opam" ] in
  with_archive (tar_gz [ F (path, "data") ]) (fun p ->
      let p = provider p in
      Alcotest.check (result Alcotest.string str_err) __LOC__ (Ok "data")
        (p.read [ "packages" ; name ; name ^ ".1.0" ; "opam" ]))

let empty () =
  with_archive (tar_gz []) (fun p ->
      let p = provider p in
      Alcotest.check (result items str_err) __LOC__ (Ok []) (p.read_dir []) ;
      Alcotest.check Alcotest.bool __LOC__ false (p.exists [ "repo" ]))

let rejected name entries =
  with_archive (tar_gz entries) (fun p ->
      Alcotest.check Alcotest.bool name true (Result.is_error p))

let bad_entries () =
  rejected "parent directory" [ F ("packages/../../etc/passwd", "") ] ;
  rejected "absolute path" [ F ("/etc/passwd", "") ] ;
  rejected "symbolic link" [ L ("packages/foo", "/etc") ] ;
  rejected "file and directory" [ F ("packages/foo", "") ; F ("packages/foo/opam", "") ] ;
  rejected "directory and file" [ F ("packages/foo/opam", "") ; F ("packages/foo", "") ]

let bad_archives () =
  with_archive "not a gzip file" (fun p ->
      Alcotest.check Alcotest.bool "garbage" true (Result.is_error p)) ;
  let data = tar_gz files in
  with_archive (Stdlib.String.sub data 0 (String.length data / 2)) (fun p ->
      Alcotest.check Alcotest.bool "truncated" true (Result.is_error p)) ;
  Alcotest.check Alcotest.bool "missing" true
    (Result.is_error (Conex_unix_provider.tar_gz_ro_provider "/nonexistent.tar.gz"))

let tests = [
  "files only", `Quick, files_only ;
  "with directory entries", `Quick, with_dirs ;
  "long name", `Quick, long_name ;
  "empty", `Quick, empty ;
  "bad entries", `Quick, bad_entries ;
  "bad archives", `Quick, bad_archives ;
]
