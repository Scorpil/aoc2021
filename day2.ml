exception Err of string

let parse_input raw_input =
  let transform line = 
    match String.split_on_char ' ' line with
        direction::distance::[] -> (direction, int_of_string distance)
      | _ -> raise(Err "Wrong input") in
  List.map transform raw_input

let part1_inner input =
  let aggregator (horizontal, depth) (direction, distance) = 
    match direction with
        "forward" -> (horizontal+distance, depth)
      | "down" -> (horizontal, depth+distance)
      | "up" -> (horizontal, depth-distance)
      | _ -> raise(Err "Wrong input") in
  let loc = List.fold_left aggregator (0, 0) input in
  match loc with (distance, depth) -> distance * depth

let part1 raw_input = parse_input raw_input |> part1_inner

let part2_inner input =
  let aggregator (horizontal, depth, aim) (direction, distance) = 
    match direction with
        "forward" -> (horizontal+distance, depth+aim*distance, aim)
      | "down" -> (horizontal, depth, aim + distance)
      | "up" -> (horizontal, depth, aim - distance)
      | _ -> raise(Err "Wrong input") in
    let loc  = List.fold_left aggregator (0, 0, 0) input in
    match loc with (distance, depth, _) -> distance * depth

let part2 raw_input = parse_input raw_input |> part2_inner
