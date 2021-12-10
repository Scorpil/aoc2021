exception Err of string

let find_mismatch corrupted incomplete line =
  let rec loop expected line =
    let closing_of pr = match pr with
        '(' -> ')'
      | '[' -> ']'
      | '{' -> '}'
      | '<' -> '>'
      | _ -> raise (Err "Unexpected character")
    in
    match line with
        [] -> incomplete expected
      | line_hd::line_tl -> match line_hd with
          '(' | '[' | '{' | '<' -> loop ((closing_of line_hd)::expected) line_tl
        | _ -> match expected with
            [] -> None
          | expected_hd::expected_tl ->
              if expected_hd == line_hd
              then loop expected_tl line_tl
              else corrupted line_hd
  in loop [] (line |> String.to_seq |> List.of_seq)

let part1 input =
  let score_of c = match c with
      ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137
    | _ -> raise (Err "Unexpected character") in
  let corrupted c = Some c in
  let incomplete _ = None in
  List.filter_map (find_mismatch corrupted incomplete) input
  |> List.map score_of
  |> List.fold_left (+) 0

let part2 input =
  let score_of_char c = match c with
      ')' -> 1
    | ']' -> 2
    | '}' -> 3
    | '>' -> 4
    | _ -> raise (Err "Unexpected character")
  in
  let score_of_expected line = line 
    |> List.fold_left (fun score c -> score * 5 + (score_of_char c)) 0
  in
  let corrupted _ = None in
  let incomplete expected = Some expected in
  let scores = List.filter_map (find_mismatch corrupted incomplete) input
  |> List.map score_of_expected
  |> List.sort compare
  in
  List.nth scores ((List.length scores) / 2)