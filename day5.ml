exception Err of string

type t_point = {
  x: int;
  y: int;
}

let print_point point =
  Printf.printf "(%d, %d)\n" point.x point.y

type t_line = {
  start: t_point;
  finish: t_point;
}

let print_line line =
  Printf.printf "(%d, %d) -> (%d, %d)\n" line.start.x line.start.y line.finish.x line.finish.y

let parse_input line =
  (* Finds string indexes where each number starts and ends *)
  let folder (acc, index) c = match acc with
    (s_from, s_len)::tail -> (match c with
          '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> ((s_from, s_len+1)::tail, index + 1)
        | ',' -> ((index+1, 0)::acc, index + 1)
        | '>' -> ((index+2, 0)::acc, index + 1)
        | _ -> (acc, index + 1))
    | _ -> raise (Err "Unexpected input")
    in

  let ranges_to_line ranges =
    match List.map (fun (s_from, s_len) -> int_of_string (String.sub line s_from s_len)) ranges with
        p1_y::p1_x::p2_y::p2_x::[] -> {
          start = {
            x = p1_x;
            y = p1_y;
          };
          finish = {
            x = p2_x;
            y = p2_y;
          }
        }
      | _ -> raise (Err "Unexpected input")
  in

  let (ranges, _) = String.fold_left folder ([(0, 0)], 0) line in
  List.rev ranges
  |> ranges_to_line

(* takes in a line and returns all points on a line *)
let line_to_points {start=start; finish=finish} =
    let rec inner acc = match acc with
        [] -> raise (Err "Unexpected acc value")
      | head::_ ->
        if head.x == finish.x && head.y == finish.y then acc
        else
          let point = {
            x =
              if head.x == finish.x then head.x else 
              if head.x > finish.x then head.x - 1 else
              head.x + 1;
            y =
              if head.y == finish.y then head.y else
              if head.y > finish.y then head.y - 1 else
              head.y + 1;
          } in inner (point::acc)
  in inner [start]

let solve filter raw_input = 
  let map: (t_point, int) Hashtbl.t = Hashtbl.create 1000 in
    let increment map key = match Hashtbl.find_opt map key with
        None -> Hashtbl.add map key 1
      | Some n -> Hashtbl.replace map key (n + 1)
    in
    List.map parse_input raw_input
      |> List.filter_map filter
      |> List.map line_to_points
      |> List.flatten
      |> List.iter (fun point -> increment map point);
    Hashtbl.fold (fun _ v acc ->
      if v >= 2 then acc + 1 else acc
    ) map 0

let part1 = 
  let is_stright line =
    if line.start.x == line.finish.x || line.start.y == line.finish.y then
    Some line
  else
    None
  in solve is_stright

let part2 = solve (fun x -> Some x)
  