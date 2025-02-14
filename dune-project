(lang dune 3.16)

(name thumper)

(generate_opam_files true)

(source
 (github max-legrand/thumper))

(authors "Maxwell Legrand")

(license MIT)

(package
 (name thumper)
 (synopsis "A small, lightweight, and opinionated web server")
 (depends
   ocaml dune lwt stdio base cmdliner core core_unix
   yojson async ppx_deriving async_ssl ezgzip yaml
 )
)

(package
 (name thumper_lwt)
 (synopsis "A small, lightweight, and opinionated web server")
 (depends
   ocaml dune lwt stdio base cmdliner core core_unix
   yojson async ppx_deriving async_ssl ezgzip yaml
 )
)

(package
 (name thumper_async)
 (synopsis "A small, lightweight, and opinionated web server")
 (depends
   ocaml dune lwt stdio base cmdliner core core_unix
   yojson async ppx_deriving async_ssl ezgzip yaml
 )
)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
