(lang dune 3.1)

(name weather_metadata)

(generate_opam_files true)

(source
 (github abhsrivastava/weather-metadata))

(authors "Abhishek Srivastava")

(maintainers "Abhishek Srivastava")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name weather_metadata)
 (synopsis "Download metadata for weather for trailhead co-ordinates")
 (description "Calls the weather.gov API to download metadata related to weather coordindates")
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
