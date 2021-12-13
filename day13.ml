exception Err of string

type point = int * int
type axis = X | Y
type fold = axis * int

module PointSet = Set.Make(
  struct
    let compare = compare
    type t = point
  end
)

let parse_input raw_input =
  let parse_point point =
    point |> Input.int_list_of_csv_string |> Input.pairs_of_list
  in

  let parse_fold fold =
    let (left, right) =
      fold |> String.split_on_char '=' |> Input.pairs_of_list
    in
    let ax = let c = String.get left ((String.length left) - 1) in
        if c = 'x' then X
        else if c = 'y' then Y
        else raise (Err "Unexpected input")
    in
    (ax, int_of_string right)
  in

  let (input_points, input_folds) = raw_input
  |> Listex.split ""
  |> Input.pairs_of_list
  in
  let points = List.map parse_point input_points in
  let folds = List.map parse_fold input_folds in
  (points, folds)

let print_set set =
  let point_char (x,y) = match PointSet.find_opt (x, y) set with
      None ->  ' '
    | Some _ -> '#'
  in
  Util.range 0 6
  |> List.iter (fun y ->
    Util.range 0 60
    |> List.map (fun x -> point_char (x, y))
    |> List.to_seq
    |> String.of_seq
    |> print_endline
  )

let solve points folds =
  let point_set =
    List.fold_left (fun set p -> PointSet.add p set) PointSet.empty points
  in

  let folder ps (axis, i) =
    let filter_y (x, y) =
      if y <= i then Some (x, y)
      else Some (x, 2*i - y)
    in
    let filter_x (x, y) =
      if x <= i then Some (x, y)
      else Some (2*i - x, y)
    in
      PointSet.filter_map (match axis with X -> filter_x | Y -> filter_y) ps
  in
  List.fold_left folder point_set folds

let part1 raw_input =
  let ((points: point list), (folds: fold list)) = parse_input raw_input in
  solve points [(List.hd folds)] |> PointSet.cardinal

let part2 raw_input =
let ((points: point list), (folds: fold list)) = parse_input raw_input in
let set = solve points folds in
print_set set; 0