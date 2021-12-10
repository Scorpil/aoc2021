exception Err of string

type t_point = int*int
type t_map = int array array

let input_to_map raw_input =
  let map = Array.make_matrix (List.hd raw_input |> String.length) (List.length raw_input) 0 in
  let input = List.map (fun line -> line
    |> String.to_seq
    |> Seq.map (fun c -> (Char.code c) - 48)
    |> List.of_seq
  ) raw_input
  in

  List.iteri (
    fun col_index -> List.iteri (
      fun row_index height -> map.(row_index).(col_index) <- height
    )
  ) input;
  map

let make_solution (map: t_map) =
  let width = Array.length map in
  let height = Array.length map.(0) in

  let value_at (x, y) = map.(x).(y) in

  let around (x, y) =
    (x-1, y)::(x, y+1)::(x+1, y)::(x, y-1)::[]
    |> List.filter (fun (x, y) -> x >= 0 && x < width && y >= 0 && y < height)
  in

  let is_low_point point =
    let center_value = value_at point in
    around point |> List.for_all (fun point -> center_value < (value_at point))
  in

  let track_basin (initial_point: t_point) =
    let basin = Hashtbl.create 100 in
    let bank = Hashtbl.create 100 in

    let is_in_table table point =
      match Hashtbl.find_opt table point with
          None -> false
        | Some _ -> true in

    let is_in_basin = is_in_table basin in
    let is_in_bank = is_in_table bank in
    let is_known point = (is_in_basin point) || (is_in_bank point) in
    let is_not_known point = Bool.not (is_known point) in

    let add_to_table table point = Hashtbl.replace table point true in
    let add_to_basin = add_to_table basin in
    let add_to_bank = add_to_table bank in

    let rec loop (edges: t_point list) =
      let append_to_edges new_edges = new_edges @ edges in
      match edges with
        [] -> basin
      | point::tail_edges ->
        if is_known point
        then loop tail_edges
        else
          if (value_at point) != 9 then (
            add_to_basin point;
            around point |> List.filter is_not_known |> append_to_edges |> loop)
          else (
            add_to_bank point;
            loop tail_edges)
    in

    around initial_point |> loop
  in

  let rec find_low_points acc maybe_point =
    let next (x, y) =
      if x == (width - 1) && y == (height - 1) then None else
      if x == (width - 1) then Some (0, y + 1) else
      Some(x+1, y)
    in
    match maybe_point with
        None -> acc
      | Some point ->
          if is_low_point point
          then
            find_low_points (point::acc) (next point)
          else find_low_points acc (next point)
  in
    let low_points = find_low_points [] (Some (0, 0)) in
    (value_at, low_points, track_basin)

let part1 raw_input =
  let (value_at, low_points, _ ) = make_solution (input_to_map raw_input) in
  (List.fold_left (+) 0 (low_points |> List.map value_at)) + (List.length low_points)

let part2 raw_input =
  let (_, low_points, track_basin) = make_solution (input_to_map raw_input) in
  let sorted_basin_sizes = List.map track_basin low_points
  |> List.map (fun b -> Hashtbl.length b)
  |> List.sort (fun b1 b2 -> -compare b1 b2) in
  match sorted_basin_sizes with
      b1::b2::b3::_ -> b1*b2*b3
    | _ -> raise (Err "Enexpected input")