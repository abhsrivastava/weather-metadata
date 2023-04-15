open Weather_metadata.Weather
open Weather_metadata.Data

let () = 
  get_metadata() 
  |> Lwt.map Metadata.to_json_list
  |> Lwt_main.run
  |> Yojson.Basic.pretty_to_string
  |> print_endline