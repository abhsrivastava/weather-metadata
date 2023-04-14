(lang dune 3.1)

(name weather_metadata)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name weather_metadata)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends 
  ocaml 
  dune
  yojson
  cohttp-lwt-unix
  cohttp-async
  )
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
