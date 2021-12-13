exception ValueError of string

let split on lst =
  let rec splitter acc lst =
  match acc with
      [] -> raise (ValueError "Empty acc")
    | hd_acc::tl_acc -> match lst with
        [] -> (List.rev hd_acc)::tl_acc
      | hd::tl ->
        if hd = on then splitter ([]::(List.rev hd_acc)::tl_acc) tl
        else splitter ((hd::hd_acc)::tl_acc) tl
  in
  splitter ([]::[]) lst |> List.rev
