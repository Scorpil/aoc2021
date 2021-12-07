let parse_input raw_input =
  let folder (lst, sum) s =
    let x = int_of_string s in
    (x::lst, x+sum)
  in
  List.hd raw_input
  |> String.split_on_char ','
  |> List.fold_left folder ([], 0)

let optimize cost_function lst initial_guess =
  let step lst current_cost guess delta =
    let new_guess_cost = cost_function lst guess in
    let slope = (new_guess_cost - current_cost) / delta in
    let new_delta = if slope > 0 then -1 else 1 in
    (new_guess_cost, new_delta)
  in

  let rec inner lst current_cost guess delta=
    let (new_guess_cost, new_delta) = step lst current_cost guess delta in
    if new_guess_cost > current_cost then current_cost
    else inner lst new_guess_cost (guess + new_delta) new_delta
  in
  let initial_guess_cost = cost_function lst initial_guess in
  let delta = 1 in
  let (_, new_delta) = step lst initial_guess_cost (initial_guess + delta) delta in
  inner lst initial_guess_cost (initial_guess + new_delta) new_delta

let solve cost_function raw_input =
  let (input, sum) = parse_input raw_input in
  let avg = sum / (List.length input) in
  optimize cost_function input avg

let part1 =
  let cost lst guess =
    List.map (fun x -> Int.abs (guess - x)) lst
    |> List.fold_left (+) 0
  in solve cost

let part2 =
  let sum_of_int x = x*(x+1)/2 in
  let cost lst guess =
    List.map (fun x ->  sum_of_int (Int.abs (guess - x))) lst
      |> List.fold_left (+) 0
  in solve cost