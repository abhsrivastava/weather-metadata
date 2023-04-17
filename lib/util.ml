open Data

let round n p = 
  Float.(
    let x = pow 10. (float_of_int p) in 
    let t = round (n *. x) in 
    t /. x
  )

let to_string (n: float) : string = 
  round n 4 |> string_of_float

let split size input = 
  let create_result result str =
    let hd = result |> List.hd in 
    let tl = result |> List.tl in 
    match hd with 
    | Process(l) when l |> List.length <= (size - 1) -> Process (str :: l) :: tl
    | Process (_) -> Process([str]) :: Wait :: hd :: tl
    | Wait -> Process([]) :: hd :: tl
  in
  input |> List.fold_left create_result [Process([])]

  let remove_none list = 
    list |> List.fold_left 
    (fun result x -> match x with 
    | Some(x) -> x :: result
    | _ -> result
    ) []
  
  let print_list (list: 'a list) (fn: 'a -> string) : unit = 
    list 
    |> List.fold_left (fun result x -> result ^ "," ^ fn x) "" 
    |> print_endline
