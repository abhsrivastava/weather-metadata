open Weather_metadata.Weather

let () = 
  get_metadata() 
  |> Lwt.map (fun list -> Metadata.to_json_list list)
  |> Lwt_main.run
  |> Yojson.Basic.pretty_to_string
  |> print_endline