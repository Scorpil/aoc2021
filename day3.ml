exception Err of string

let rec calculate_score input scores =
  let bin_to_bits bin =
    List.init (String.length bin) (fun pos -> int_of_string (String.sub bin pos 1)) in
  let bin_score bin scores =
    List.combine bin scores
    |> List.map (fun (bit, score) -> score + bit) in
  match input with
    [] -> scores
  | bin::tail -> calculate_score tail (bin_score (bin_to_bits bin) scores)

let part1 input =
  let initial_scores = List.init (String.length (List.hd input)) (fun _ -> 0) in
  let threshold = (List.length input) / 2 in
  let build_high_low (high, low) score =
    if score > threshold then
        (high lsl 1 + 1, low lsl 1)
      else
        (high lsl 1, low lsl 1 + 1) in
  let (high, low) = calculate_score input initial_scores
    |> List.fold_left build_high_low (0, 0) in
  high * low

let part2 _input = 
  (* TODO *)
  2