exception Err of string

let parse_input raw_input = match raw_input with
    first_line::[] ->
      String.split_on_char ',' first_line
      |> List.map int_of_string
  | _ -> raise (Err "Unexpected input")

let rec calc days cur_c ic c =
  if days <= cur_c then
    1
  else
    calc (days - cur_c) c ic c +
    calc (days - cur_c) ic ic c

let ht_create days =
  let ht: ((int, int) Hashtbl.t) = Hashtbl.create 9 in
  Hashtbl.add ht 1 (calc (days+1) (1+1) 9 7);
  Hashtbl.add ht 2 (calc (days+1) (2+1) 9 7);
  Hashtbl.add ht 3 (calc (days+1) (3+1) 9 7);
  Hashtbl.add ht 4 (calc (days+1) (4+1) 9 7);
  Hashtbl.add ht 5 (calc (days+1) (5+1) 9 7);
  Hashtbl.add ht 6 (calc (days+1) (6+1) 9 7);
  Hashtbl.add ht 7 (calc (days+1) (7+1) 9 7);
  Hashtbl.add ht 8 (calc (days+1) (8+1) 9 7);
  ht

let solve input days = 
  let ht = ht_create days in
  List.map (fun x -> Hashtbl.find ht x) input
  |> List.fold_left (+) 0

let part1 raw_input = solve (parse_input raw_input) 80
let part2 raw_input = solve (parse_input raw_input) 256