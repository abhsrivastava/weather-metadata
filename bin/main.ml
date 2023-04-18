open Weather_metadata.Weather
open Weather_metadata.Data

let () = 
  let inputJson = Yojson.Basic.from_file "input.json" in
  let outputJson = get_metadata(inputJson) 
  |> Lwt.map Metadata.to_json_list
  |> Lwt_main.run
  |> Yojson.Basic.pretty_to_string in 
  let file = open_out "trail_database.json" in 
  outputJson |> Printf.fprintf file "%s\n";
  file |> close_out