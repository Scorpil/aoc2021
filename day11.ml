let parse_input raw_input =
  let height = List.length raw_input in
  let width = String.length (List.hd raw_input) in
  let parse_char c = let ascii_shift = 48 in (int_of_char c) - ascii_shift in
  String.concat "" raw_input
  |> String.to_seq
  |> Seq.map parse_char
  |> Grid.of_seq width height

let step grid =
  let flashes: int ref = { contents = 0 } in
  let zeros: Grid.point list ref = { contents = [] } in
  let mapper point value =
    if value == 9 then (
      zeros := (point::!zeros);
      flashes := !flashes + 1;
      0
    ) else
      value + 1
  in
  let grid = Grid.mapi mapper grid in
  let rec resolve_zeros zeros = match zeros with
      [] -> ()
    | point::zero_tail ->
      let incr acc point =
        let value = Grid.get grid point in
        if value == 0 then acc else
        if value == 9 then (
          Grid.set grid point 0;
          flashes := !flashes + 1;
          (point::acc)
        ) else (
          Grid.set grid point (value + 1);
          acc
        )
      in
      let new_zeros =
        Grid.around grid point
        |> List.fold_left incr []
      in
      resolve_zeros (zero_tail @ new_zeros)
  in
  resolve_zeros !zeros;
  grid, !flashes

let part1 raw_input =
  let grid = parse_input raw_input in
  let rec loop grid flashes start finish =
    if start == finish then flashes
    else
      let (new_grid, new_flashes) = step grid in
      loop new_grid (flashes + new_flashes) (start+1) finish
  in
  loop grid 0 0 100

let part2 raw_input =
  let grid = parse_input raw_input in
  let rec loop grid i =
    if Grid.for_all (fun v -> v == 0) grid then i
    else
      let (new_grid, _) = step grid in
      loop new_grid (i+1)
  in
  loop grid 0