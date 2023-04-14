open Database
open Lwt
open Cohttp_lwt_unix

module Metadata = struct
  type t = {
    gridId: string;
    gridX: int;
    gridY: int;
    forecastHourly: string;
    forecastZone: string;
    county: string;
    radarStation: string;
    city: string;}
  let to_string t = 
    "{ " ^ 
      "gridId=" ^ t.gridId ^ "; " ^ 
      "gridX=" ^ (Int.to_string t.gridX) ^ "; " ^ 
      "gridY=" ^ (Int.to_string t.gridY) ^ "; " ^ 
      "forecastHourly=" ^ t.forecastHourly ^ "; " ^ 
      "forecastZone=" ^ t.forecastZone ^ "; " ^ 
      "county=" ^ t.county ^ "; " ^ 
      "radarStation=" ^ t.radarStation ^ "; " ^ 
      "city=" ^ t.city ^ "; " ^ 
  "}"
  let to_json t = 
    `Assoc [
      ("gridId", `String t.gridId); 
      ("gridX", `Int t.gridX); 
      ("gridY", `Int t.gridY); 
      ("forecastHourly", `String t.forecastHourly); 
      ("forecastZone", `String t.forecastZone);
      ("county", `String t.county);
      ("radarStation", `String t.radarStation);
      ("city", `String t.city)
    ]

let to_json_list list =
    `List (list |> List.map to_json)
end

let buildUrl (lat, long) : string =
  "https://api.weather.gov/points/" ^ (Util.to_string lat) ^ "," ^ (Util.to_string long)

let make_http_call tup = 
  let url = (buildUrl tup) in 
  Printf.printf "Going to call %s \n" url;
  url 
  |> Uri.of_string
  |> Client.get
  >>= fun (resp, body) -> 
    let code = resp |> Response.status |> Cohttp.Code.code_of_status in
    Printf.printf "Response code: %d\n" code;
    body |> Cohttp_lwt.Body.to_string

let get_metadata_for_trails () : string list Lwt.t =
  trailsList 
  |> Util.split 50 
  |> List.map (fun batch -> Lwt.bind (Lwt_unix.sleep 1.0) (fun _ -> batch |> List.map (make_http_call) |> Lwt.all)) 
  |> Lwt.all 
  |> Lwt.map(List.flatten)

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
  |> Lwt.map (List.map (parse_json))


