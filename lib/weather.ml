open Database
open Lwt
open Cohttp_lwt_unix
open Data

let buildUrl (lat, long) : string =
  "https://api.weather.gov/points/" ^ (Util.to_string lat) ^ "," ^ (Util.to_string long) |> String.trim

let make_http_call ?(wait=15) = function 
  | Process(list) -> 
    list 
    |> List.map(Uri.of_string)
    |> Lwt_list.map_s (fun uri -> 
      Lwt.catch 
        (fun () -> uri |> Client.get |> Lwt.map(Option.some)) 
        (function 
        | exn -> "\nReceived Exception for url: " ^ (uri |> Uri.to_string) ^ "\nException: " ^ (exn |> Printexc.to_string) |> Lwt_io.(write stdout) |> Lwt.map(fun _ -> None)
        )
    )
    |> Lwt.map(Util.remove_none)
    >>= (fun list -> 
      list |> Lwt_list.map_s(fun (resp, body) -> 
        let code = resp |> Response.status |> Cohttp.Code.code_of_status in
        if code == 200 then 
          Lwt.catch 
            (fun () -> body |> Cohttp_lwt.Body.to_string |> Lwt.map (Option.some))
            (function
              | exn -> exn |> Printexc.to_string |> Lwt_io.(write stdout) |> Lwt.map ( fun _ -> None)
            )
        else  
          "did not get 200 response" |> Lwt_io.(write stdout) |> Lwt.map ( fun _ -> None )
      )
    )
  | Wait -> 
    print_endline("going to wait now for " ^ (wait |> Int.to_string) ^ " seconds");
    Unix.sleep(wait); 
    [None] |> Lwt.return

let get_metadata_for_trails () =
  trailsList 
  |> List.map buildUrl
  |> Util.split 10
  |> Lwt_list.map_s(make_http_call) (* returns string option list list Lwt :) *)
  |> Lwt.map(List.flatten) (* now we are down to string option list lwt thanks to flatten *)
  |> Lwt.map (Util.remove_none) (* now we are down to string list lwt thanks to remove none *)
  
let parse_json json = 
  Metadata.(
    Yojson.Basic.Util.(
      {
        gridId = json |> member "properties" |> member "gridId" |> to_string;
        gridX = json |> member "properties" |> member "gridX" |> to_int;
        gridY = json |> member "properties" |> member "gridY" |> to_int;
        forecastHourly = json |> member "properties" |> member "forecastHourly" |> to_string;
        forecastZone = json |> member "properties" |> member "forecastZone" |> to_string;
        county = json |> member "properties" |> member "county" |> to_string;
        radarStation = json |> member "properties" |> member "radarStation" |> to_string;
        city = json |> member "properties" |> member "relativeLocation" |> member "properties" |> member "city" |> to_string;
      }
    )
  )

let get_metadata () = 
  get_metadata_for_trails() 
  |> Lwt.map (List.map Yojson.Basic.from_string)
  |> Lwt.map (List.map parse_json)
