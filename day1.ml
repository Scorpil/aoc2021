exception Err of string

let fun_decidor a b =
  if a < b then (fun(x) -> x + 1) else (fun(x) -> x)

let part1_inner input = match input with
    [] -> raise(Err "No input")
  | head::tail ->
      let folder (prev, acc) cur = (cur, if cur > prev then acc + 1 else acc) in
      let result = List.fold_left folder (head, 0) tail in
      match result with (_, acc) -> acc

let part1 raw_input = List.map int_of_string raw_input |> part1_inner

let part2 raw_input =
  let input = List.map int_of_string raw_input in
  let rec calculate_windows input windows = match input with
      [] -> windows
    | _::[] -> windows
    | _::_::[] -> windows
    | a::b::c::tail -> calculate_windows (b::c::tail) ((a+b+c)::windows) in
    calculate_windows input []
    |> List.rev
    |> part1_inner
