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

type list_type =
| Process of string list
| Wait 