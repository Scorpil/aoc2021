exception Err of string

let parse_input raw_input = 
  let graph = Hashtbl.create (List.length raw_input) in
  let parse_line line = match String.split_on_char '-' line with
      a::b::[] ->
        Hashtbl.add graph a b;
        Hashtbl.add graph b a;
    | _ -> raise (Err "Unexpected input")
  in
  List.iter parse_line raw_input;
  graph

let str_list lst = Printf.sprintf "%s" (String.concat "," lst)

let traverse jokers graph =
  let rec explore jokers blocked path = match path with
      [] -> raise (Err "Empty path")
    | current::_ ->
        if (current = "end") then [String.concat "," path]
        else
          let not_blocked destination = match List.find_opt (fun blocked -> blocked = destination) blocked with None -> true | Some _ -> false in
          let destinations =
            Hashtbl.find_all graph current
            |> List.filter not_blocked
          in
          if (List.length destinations) = 0 then []
          else
            List.map (fun destination ->
              let is_big_cave = (String.uppercase_ascii current) = current in
              if is_big_cave then explore jokers blocked (destination::path)
              else
                if (current = "start") || (jokers = 0) then
                  explore jokers (current::blocked) (destination::path) 
                else
                  ((explore (jokers-1) blocked (destination::path)) @
                  (explore jokers (current::blocked) (destination::path)))
                  |> List.sort_uniq compare
                
            )
            destinations
            |> List.flatten
  in
  explore jokers [] ["start"]

let part1 raw_input = traverse 0 (parse_input raw_input) |> List.length

let part2 raw_input = traverse 1 (parse_input raw_input) |> List.length