let round n p = 
  Float.(
    let x = pow 10. (float_of_int p) in 
    let t = round (n *. x) in 
    t /. x
  )

let to_string (n: float) : string = 
  round n 4 |> string_of_float

let split size list = 
  let create_result result str =
    let hd = result |> List.hd in 
    let tl = result |> List.tl in 
    if hd |> List.length <= size - 1 then
      (str :: hd) :: tl
    else
      [str] :: result in
  list |> List.fold_left create_result [[]]