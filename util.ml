let rec frange stop_func step_func i  =
  if stop_func i
  then []
  else
    i::(frange stop_func step_func (step_func i))

let range_step_inc start finish step =
  let stop_func i = i >= finish in
  let step_func i = i + step in
  frange stop_func step_func start

let range_step_dec start finish step =
  let stop_func i = i <= finish in
  let step_func i = i - step in
  frange stop_func step_func start

let range_step start finish step =
  let func = if start <= finish then range_step_inc else range_step_dec in
  func start finish step

let range start finish = range_step start finish 1