open Database
open Lwt
open Cohttp_lwt_unix
open Data

let buildUrl (lat, long) : string =
  "https://api.weather.gov/points/" ^ (Util.to_string lat) ^ "," ^ (Util.to_string long)

let make_http_call = function 
  | Process(list) -> 
    list |> List.map (fun url -> 
      Printf.printf "Going to call %s \n" url;
      url 
      |> Uri.of_string
      |> Client.get
      >>= fun (resp, body) -> 
        let code = resp |> Response.status |> Cohttp.Code.code_of_status in
        Printf.printf "Response code: %d\n" code;
        body |> Cohttp_lwt.Body.to_string |> Lwt.map (fun x -> Some(x))
    )
  | Wait -> 
    Printf.printf "%s\n" "waiting :)";
    [Lwt_unix.sleep 3.0] |> List.map (Lwt.map (fun _ -> None))

let get_metadata_for_trails () =
  trailsList 
  |> List.map buildUrl
  |> Util.split 50 
  |> List.map (make_http_call)
  |> List.flatten
  |> Lwt.all
  |> Lwt.map (Util.remove_none)
  
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


