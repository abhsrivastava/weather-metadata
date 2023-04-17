open Weather_metadata.Weather
open Weather_metadata.Data

let () = 
  let json = get_metadata() 
  |> Lwt.map Metadata.to_json_list
  |> Lwt_main.run
  |> Yojson.Basic.pretty_to_string in 
  let file = open_out "trail_database.json" in 
  json |> Printf.fprintf file "%s\n";
  file |> close_out