exception Err of string

module CharSet = Set.Make(Char)

let parse_line raw_line =
  let parse_line_part input_part =
    String.trim input_part
    |> String.split_on_char ' '
    |> List.map (fun s -> String.to_seq s |> CharSet.of_seq)
  in
  String.split_on_char '|' raw_line
    |> List.map parse_line_part

let part1 raw_input =
    let filter2345 s = match (CharSet.cardinal s) with
        2 | 3 | 4 | 7 -> true
      | _ -> false
    in
    let count_single_input raw_line =
      match parse_line raw_line with
        _::results::[] -> List.filter filter2345 results
        | _ -> raise (Err "Unexpected input");
      |> List.length
    in
      (List.map count_single_input raw_input)
      |> List.fold_left (+) 0

let part2 raw_input =
  let solve_line raw_line =

    let build_decoder signal =
      (* set of characters -> digit *)
      let decoding_map = Hashtbl.create 10 in

      (* digit -> set of characters *)
      let encoding_arr = Array.make 10 CharSet.empty in

      (* associate set to digit *)
      let assoc set digit =
        Hashtbl.replace decoding_map (CharSet.elements set) digit;
        Array.set encoding_arr digit set;
      in

      let encode digit =
        Array.get encoding_arr digit
      in

      let decode set =
        Hashtbl.find decoding_map (CharSet.elements set)
      in

      (* decode digits of unique encoding lengths:
          len 2 => 1, len 3 => 7, len 4 = 4, len 7 => 8 *)
      let rec phase1 input = match input with
          [] -> []
        | set::tail -> match CharSet.cardinal set with
              2 -> assoc set 1; phase1 tail
            | 3 -> assoc set 7; phase1 tail
            | 4 -> assoc set 4; phase1 tail
            | 7 -> assoc set 8; phase1 tail
            | _ -> set::(phase1 tail)
      in

      (* decode 3 and 6
          3 has length 5 and includes 1
          6 has length 6 and does not include 1 *)
      let rec phase2 input = match input with
          [] -> []
        | set::tail -> match CharSet.cardinal set with
            5 ->
              if CharSet.subset (encode 1) set
              then (assoc set 3; phase2 tail)
              else set::(phase2 tail)
          | 6 ->
              if Bool.not (CharSet.subset (encode 1) set)
              then (assoc set 6; phase2 tail)
              else set::(phase2 tail)
          | _ -> set::(phase2 tail)
      in

      (* decode 9 and 5
          5 has length 5 and 6 indudes 5
          9 has length 6 and includes 3 *)
      let rec phase3 input = match input with
          [] -> []
        | set::tail -> match CharSet.cardinal set with
              5 ->
                if CharSet.subset set (encode 6)
                then (assoc set 5; phase3 tail)
                else set::(phase3 tail)
            | 6 ->
                if CharSet.subset (encode 3) set
                then (assoc set 9; phase3 tail)
                else set::(phase3 tail)
            | _ -> set::(phase3 tail)
      in

      (* decode 2 (length 5) and 0 (length 6) *)
      let rec phase4 input = match input with
          [] -> []
        | set::tail -> match CharSet.cardinal set with
            5 -> (assoc set 2; phase4 tail)
          | 6 -> (assoc set 0; phase4 tail)
          | _ -> raise (Err "Unexpected input")
      in

      let _ = signal |> phase1 |> phase2 |> phase3 |> phase4 in
      let decoder encoded_output =
        List.map decode encoded_output
      in
      decoder
    in

    let digit_list_to_number digits =
      List.fold_left (fun acc d -> acc * 10 + d) 0 digits
    in

    match parse_line raw_line with
        signal::encoded_output::[] ->
          let decoder = build_decoder signal in
          decoder encoded_output |> digit_list_to_number
      | _ -> raise (Err "Unexpected input")
  in

  List.map solve_line raw_input
  |> List.fold_left (+) 0