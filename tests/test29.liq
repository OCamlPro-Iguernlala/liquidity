[%%version 0.14]

let%entry main
    (parameter : (int, unit) contract * int list)
    (storage : int)
  : unit * int =

  let (c, l) = parameter in
  match l with
  | [] ->
    ((), storage)
  | x :: _ ->
    let (result, storage) = Contract.call c 0tz storage x in
    (result, storage)
