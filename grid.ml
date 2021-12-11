type point = int*int
type grid = {
  arr: int array;
  width: int;
  height: int;
}

let make width height arr = {
  arr = arr;
  width = width;
  height = height;
}

let of_seq width height seq =
  let arr = Array.make (width*height) 0 in
  let _ = Seq.fold_left (fun i n -> Array.set arr i n; i+1) 0 seq in
  make width height arr

let to_index grid ((x, y): point) = y * grid.height + x
let to_point grid i =
  let y = i / grid.width in
  let x = i - y * grid.width in
  (x, y)
let size grid = grid.width * grid.height

let set grid point value = Array.set grid.arr (to_index grid point) value

let around grid ((x, y): point) =
  let is_valid (x, y) = x >= 0 && x < grid.width && y >= 0 && y < grid.height in
  (x+1, y)::
  (x+1, y+1)::(x, y+1)::(x-1, y+1)::
  (x-1, y)::
  (x-1, y-1)::(x, y-1)::(x+1, y-1)::
  [] |> List.filter is_valid
let get grid point = Array.get grid.arr (to_index grid point)
let iteri f grid = Array.iteri (fun i v -> f (to_point grid i) v) grid.arr
let fold_left f acc grid = Array.fold_left (fun acc v -> f acc v) acc grid.arr
let mapi f grid
 =
  {
    arr =  Array.mapi (fun i v -> f (to_point grid i) v) grid.arr;
    width = grid.width;
    height = grid.height;
  }
let for_all f grid = Array.for_all f grid.arr

let to_string grid = 
  let _, acc = fold_left (fun (i, acc) v -> (
    i+1,
    if (Int.rem i grid.height) == 0
    then ((string_of_int v)::"\n"::acc)
    else ((string_of_int v)::acc))
  ) (0, []) grid in
  acc
  |> List.rev
  |> String.concat ""