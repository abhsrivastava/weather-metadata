module Metadata = struct
  type t = {
    id: string;
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
      "id=" ^ t.id ^ "; " ^
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
      ("id", `String t.id);
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
    `List (list |> List.map(to_json))
end

module InputData = struct
  type t = {
    latitude: float;
    longitude: float;
    trailhead: string;
    trailheadUrl: string option;
  }
  let to_string t = 
    "{ " ^ 
      "latitude=" ^ (t.latitude |> Float.to_string) ^ "; " ^
      "longitude=" ^ (t.longitude |> Float.to_string) ^ "; " ^ 
      "trailhead=" ^ t.trailhead ^ "; " ^ 
      (if t.trailheadUrl |> Option.is_some then 
        "trailheadUrl=" ^ (t.trailheadUrl |> Option.value ~default:"") ^ "; "
      else 
        "") ^ 
    "}"
    
  let to_json t = 
    let latitudeTuple = ("latitude", `Float t.latitude) in 
    let longitudeTuple = ("longitude", `Float t.longitude) in
    let trailheadTuple = ("trailhead", `String t.trailhead) in 
    let fieldsList = [latitudeTuple; longitudeTuple; trailheadTuple] in
    `Assoc (Option.fold t.trailheadUrl 
      ~none:fieldsList 
      ~some: (fun v -> List.concat [fieldsList; [("trailheadUrl", `String v)]]))

  let to_json_list list = 
    `List(list |> List.map(to_json))
end