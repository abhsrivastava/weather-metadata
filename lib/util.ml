let round n p = 
  Float.(
    let x = pow 10. (float_of_int p) in 
    let t = round (n *. x) in 
    t /. x
  )

let to_string (n: float) : string = 
  round n 4 |> string_of_float

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
