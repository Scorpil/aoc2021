exception ValueError of string

let int_list_of_csv_string line =
  String.split_on_char ',' line
  |> List.map int_of_string

let pairs_of_list lst =
  match lst with
    a::b::[] -> (a, b)
  | _ -> raise (ValueError "Failed to parse into pair")