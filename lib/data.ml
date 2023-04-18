module Metadata = struct
  type t = {
    id: string;
    latitude: float;
    longitude: float;
    metadataUrl: string;
    trailhead: string;
    trailheadUrl: string option;
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
      "id='" ^ t.id ^ "'; " ^
      "latitude=" ^ (t.latitude |> Float.to_string) ^ ";" ^
      "longitude="^ (t.longitude |> Float.to_string) ^ ";" ^
      "metadataUrl='" ^ t.metadataUrl ^ "'; " ^
      "trailhead='" ^ t.trailhead ^ "'; " ^
      (t.trailheadUrl |> Option.fold ~none:"" ~some:(fun v -> "trailhead-url='" ^ v ^ "'; ")) ^
      "gridId='" ^ t.gridId ^ "'; " ^ 
      "gridX=" ^ (Int.to_string t.gridX) ^ "; " ^ 
      "gridY=" ^ (Int.to_string t.gridY) ^ "; " ^ 
      "forecastHourly='" ^ t.forecastHourly ^ "'; " ^ 
      "forecastZone='" ^ t.forecastZone ^ "'; " ^ 
      "county='" ^ t.county ^ "'; " ^ 
      "radarStation='" ^ t.radarStation ^ "'; " ^ 
      "city='" ^ t.city ^ "'; " ^ 
    "}"
  let to_json t = 
    let fields = [
      ("id", `String t.id);
      ("gridId", `String t.gridId); 
      ("latitude", `Float t.latitude);
      ("longitude", `Float t.longitude);
      ("metadata-url", `String t.metadataUrl);
      ("trailhead", `String t.trailhead);
      ("gridX", `Int t.gridX); 
      ("gridY", `Int t.gridY); 
      ("forecastHourly", `String t.forecastHourly); 
      ("forecastZone", `String t.forecastZone);
      ("county", `String t.county);
      ("radarStation", `String t.radarStation);
      ("city", `String t.city)
    ] in 
    `Assoc (t.trailheadUrl |> Option.fold ~none:fields ~some:(fun v -> List.concat([fields; [("trailhead-url", `String v)]])))
    
  let to_json_list list =
    `List (list |> List.map(to_json))
end

module InputData = struct
  type t = {
    latitude: float;
    longitude: float;
    metadataUrl: string;
    trailhead: string;
    trailheadUrl: string option;
  }
  let to_string t = 
    "{ " ^ 
      "latitude=" ^ (t.latitude |> Float.to_string) ^ "; " ^
      "longitude=" ^ (t.longitude |> Float.to_string) ^ "; " ^ 
      "metadataUrl" ^ t.metadataUrl ^ ";" ^
      "trailhead=" ^ t.trailhead ^ "; " ^ 
      (if t.trailheadUrl |> Option.is_some then 
        "trailheadUrl=" ^ (t.trailheadUrl |> Option.value ~default:"") ^ "; "
      else 
        "") ^ 
    "}"
    
  let to_json t = 
    let latitudeTuple = ("latitude", `Float t.latitude) in 
    let longitudeTuple = ("longitude", `Float t.longitude) in
    let metadataUrlTuple = ("metadataUrl", `String t.metadataUrl) in
    let trailheadTuple = ("trailhead", `String t.trailhead) in 
    let fieldsList = [latitudeTuple; longitudeTuple; metadataUrlTuple; trailheadTuple] in
    `Assoc (Option.fold t.trailheadUrl 
      ~none:fieldsList 
      ~some: (fun v -> List.concat [fieldsList; [("trailheadUrl", `String v)]]))

  let to_json_list list = 
    `List(list |> List.map(to_json))
  
  let from_json t =
    let open Yojson.Basic.Util in 
    t |> to_list |> List.map(fun x -> 
      let latitude = x |> member "latitude" |> to_float in 
      let longitude = x |> member "longitude" |> to_float in 
      let trailhead = x |> member "trailhead" |> to_string in 
      let trailheadUrlOpt = x |> member "trailead_url" |> to_string_option in 
      {
        latitude = latitude;
        longitude = longitude;
        metadataUrl = "https://api.weather.gov/points/" ^ (latitude |> Float.to_string) ^ "," ^ (longitude |> Float.to_string);
        trailhead = trailhead;
        trailheadUrl = trailheadUrlOpt;
      }
    )
end