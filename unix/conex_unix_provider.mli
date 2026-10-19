(** Unix IO provider *)

(** [fs_provider path] is a data provider backed by a file system or error. *)
val fs_provider : string -> (Conex_io.t, string) result

(** [fs_ro_provider path] is a read-only data provider backed by a file system
    or error. *)
val fs_ro_provider : string -> (Conex_io.t, string) result

(** [tar_gz_ro_provider filename] is a read-only data provider backed by the
    gzipped tar archive [filename] (as stored by opam >= 2.6 for http
    repositories), or error.  The archive is read into memory. *)
val tar_gz_ro_provider : string -> (Conex_io.t, string) result
