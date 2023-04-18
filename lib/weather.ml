open Lwt
open Cohttp_lwt_unix
open Data

module RateLimiter = Lwt_throttle.Make(
  struct
    type t = string
    let hash = Hashtbl.hash
    let equal s1 s2 = hash s1 == hash s2
  end
)

let make_http_call (inputObject: InputData.t) client =
    let uri = inputObject.metadataUrl |> Uri.of_string in
    Lwt.catch 
      (fun () -> uri |> client |> Lwt.map(Option.some)) 
      (function 
      | exn -> 
        "\nReceived Exception for url: " ^ 
        (uri |> Uri.to_string) ^ 
        "\nException: " ^ (exn |> Printexc.to_string) 
        |> Lwt_io.(write stdout) 
        |> Lwt.map(fun _ -> None)
      )
    >>= (
      function
      | Some(resp, body) -> 
          let code = resp |> Response.status |> Cohttp.Code.code_of_status in
          if code == 200 then 
            Lwt.catch 
              (fun () -> body |> Cohttp_lwt.Body.to_string |> Lwt.map (fun body -> Some((inputObject, body))))
              (function
              | exn -> exn |> Printexc.to_string |> Lwt_io.(write stdout) |> Lwt.map ( fun _ -> None)
              )
          else
            "\nDid not get 200 response for url: " ^ inputObject.metadataUrl ^ "\nInstead got response " ^ (code |> Int.to_string) 
            |> Lwt_io.(write stdout) 
            |> Lwt.map ( fun _ -> None )  
      | None -> () |> Lwt.return |> Lwt.map(fun _ -> None)  
    )

let get_metadata_for_trails (inputObjectList: InputData.t list) client =
  inputObjectList
  |> Lwt_list.map_s(fun (inputObject: InputData.t) -> make_http_call inputObject client) (* returns string option list Lwt *)
  |> Lwt.map (Util.remove_none) (* now we are down to string list lwt thanks to remove none *)
  
let parse_json (inputObject: InputData.t) json = 
  try 
  Metadata.(
    Yojson.Basic.Util.(
      {
        id = json |> member "id" |> to_string_option |> Option.value ~default:"";
        latitude = inputObject.latitude;
        longitude = inputObject.longitude;
        metadataUrl = inputObject.metadataUrl;
        trailhead = inputObject.trailhead;
        trailheadUrl = inputObject.trailheadUrl;
        gridId = json |> member "properties" |> member "gridId" |> to_string_option |> Option.value ~default:"";
        gridX = json |> member "properties" |> member "gridX" |> to_int_option |> Option.value ~default:0;
        gridY = json |> member "properties" |> member "gridY" |> to_int_option |> Option.value ~default:0;
        forecastHourly = json |> member "properties" |> member "forecastHourly" |> to_string_option |> Option.value ~default:"";
        forecastZone = json |> member "properties" |> member "forecastZone" |> to_string_option |> Option.value ~default:"";
        county = json |> member "properties" |> member "county" |> to_string_option |> Option.value ~default:"";
        radarStation = json |> member "properties" |> member "radarStation" |> to_string_option |> Option.value ~default:"";
        city = json |> member "properties" |> member "relativeLocation" |> member "properties" |> member "city" |> to_string_option |> Option.value ~default:"";
      } |> Option.some
    )
  )
  with 
  | exn -> 
    "Failed to parse json: \n" ^ (json |> Yojson.Basic.pretty_to_string) ^
    "Exception: \n" ^ (exn |> Printexc.to_string) ^ "\n"
    |> print_endline; 
    None

let get_metadata (inputObjectList: InputData.t list) = 
  let rec throttled_get uri = 
    let limiter = RateLimiter.create ~rate:1 ~max:1 ~n:1 in 
    let channel = "https://api.weather.gov/" in   
    let open Lwt.Syntax in 
    let* allowed = RateLimiter.wait limiter channel in
    if allowed then 
      uri |> Client.get 
    else 
      let* () = Lwt_unix.sleep 1. in throttled_get uri
  in
  get_metadata_for_trails inputObjectList throttled_get
  |> Lwt.map (List.map (fun (inputObject, body) -> (inputObject, Yojson.Basic.from_string body)))
  |> Lwt.map (List.map (fun (inputObject, json) -> parse_json inputObject json))
  |> Lwt.map(Util.remove_none)
