open Weather_metadata.Weather
open Weather_metadata.Data

let () =
  let inputJson = Yojson.Basic.from_file "input.json" in
  let inputObjectList = inputJson |> InputData.from_json in
  let _ = print_endline "\nstarting processing..." in
  let outputJson =
    get_metadata inputObjectList
    |> Lwt.map Metadata.to_json_from_hashtbl
    |> Lwt_main.run |> Yojson.Basic.pretty_to_string
  in
  let file = open_out "trail_database.json" in
  outputJson |> Printf.fprintf file "%s\n";
  file |> close_out
