type most_common = | One | Zero | None;;

let take n lst =
  let rec inner acc n lst =
    if n == 0 then acc else
    match lst with
      head::tail -> inner (head::acc) (n-1) tail
    | [] -> acc
  in
    inner [] n lst |> List.rev

let calculate_most_common input initial_scores =
  let rec calculate_score_inner input scores =

    let bin_score bin scores = match bin with [] -> [] | _ ->
      List.combine (take (List.length scores) bin) scores
      |> List.map (fun (bit, score) -> score + bit) in

    match input with
      [] -> scores
    | bin::tail -> calculate_score_inner tail (bin_score bin scores) in

  let to_most_common score =
      if score * 2 < (List.length input) then Zero else
      if score * 2 == (List.length input) then None else
      One in

  calculate_score_inner input initial_scores
  |> List.map to_most_common


let process_input raw_input =
  let bin_to_bits bin = List.init (String.length bin) (fun pos -> int_of_string (String.sub bin pos 1)) in
  List.map bin_to_bits raw_input

let part1 raw_input = let input = process_input raw_input in
  let build_high_low (high, low) most_common =
    match most_common with
        One | None -> (high lsl 1 + 1, low lsl 1)
      | Zero -> (high lsl 1, low lsl 1 + 1) in
  let initial_scores = List.init (List.length (List.hd input)) (fun _ -> 0) in
  let (high, low) = calculate_most_common input initial_scores
    |> List.fold_left build_high_low (0, 0) in
  high * low

let part2 raw_input = let input = process_input raw_input in
  let rec inner criterion acc input =
    let most_common = calculate_most_common input [0] in

    let filter char input =
      let select_matching bin =
        if List.hd bin == char
        then Some(List.tl bin)
        else None in
      let res = List.filter_map select_matching input in
      res
    in
    if (List.length input) == 1 then
      List.hd input
      |> List.append (List.rev acc)
    else
      match most_common with
          [] -> acc
        | comp :: _ -> let bit = criterion(comp) in inner criterion (bit::acc) (filter bit input)
    in

    let to_dec bin =
      match bin with
        [] -> 0
      | head::tail -> List.fold_left (fun acc bit -> acc lsl 1 + bit) head tail
    in

    let ox =
      let criterion comp = match comp with
      | One | None -> 1
      | Zero -> 0 in
      inner criterion [] input |> to_dec in

    let co2 =
      let criterion comp = match comp with
      | One | None-> 0
      | Zero -> 1 in
      inner criterion [] input |> to_dec in
    co2 * ox