exception Err of string

type board_score = {
  row: (int, int) Hashtbl.t;
  col: (int, int) Hashtbl.t;
  mutable hits: int;
  mutable won: bool;
}

type game = {
  numbers: int list;
  boards: ((int, int * int) Hashtbl.t) list;
}

type state = {
  game: game;
  scores: board_score list;
  mutable code: int option;
}

let string_to_numbers s = String.split_on_char ' ' s
  |> List.filter_map int_of_string_opt

(* Build game record *)
let build_game input =

  (* Parse first row: bingo numbers *)
  let parse_numbers input = match input with
      first_row::tail ->
        let numbers = String.split_on_char ',' first_row
          |> List.filter_map int_of_string_opt in
        (numbers, tail)
    | _ -> raise (Err "Wrong input") in
  let (numbers, input) = parse_numbers input in

  (* Parse board data into boards *)
  let parse_boards input =

    (* Parse single board recrod *)
    let parse_single_board input =
      let rec inner lines (x, y) board = match lines with
          [] -> board
        | line::tail_lines -> match line with
            [] -> inner tail_lines (x+1, -1) board
          | number::tail ->
            let input_tail = tail::tail_lines in
            let new_index = (x, y+1) in
              Hashtbl.add board number new_index;
              inner input_tail new_index board
      in
        inner input (0, -1) (Hashtbl.create 25)
    in

    let rec inner boards input = match input with
        [] -> boards
      | _empty_row::l1::l2::l3::l4::l5::tail ->
        let board_lines = List.map string_to_numbers (l1::l2::l3::l4::l5::[]) in
        let new_board = parse_single_board board_lines in
          inner (new_board::boards) tail
      | _ -> raise (Err "Unexpected input") in
    inner [] input
  in
  {
    numbers = numbers;
    boards = parse_boards input;
  }

(* Build state record *)
let build_state game =
  let build_scores n =
    let build_single_score _ =
      let rec repeat x f = match x with
          0 -> ()
        | _ -> f(x-1); repeat (x-1) f
      in
      let score = {
        row = Hashtbl.create 5;
        col = Hashtbl.create 5;
        hits = 0;
        won = false;
      } in
      repeat 5 (fun n ->
        Hashtbl.add score.row n 0;
        Hashtbl.add score.col n 0;
      );
      score
    in
    List.init n build_single_score
  in
  {
    game = game;
    scores = build_scores (List.length game.boards);
    code = None
  }

let bingo state =
  let board_sum board =
    let items = Hashtbl.to_seq_keys board |> List.of_seq in
    List.fold_left (+) 0 items
  in

  let rec inner numbers state win_check =
    let update_scores number state =
      let update_single_score state number (board, score) = if score.won then () else
        match Hashtbl.find_opt board number with
          None -> ()
        | Some (row, col) ->
          let cur_row_value = Hashtbl.find score.row row in
          let cur_col_value = Hashtbl.find score.col col in
              Hashtbl.replace score.row row (cur_row_value + 1);
              Hashtbl.replace score.col col (cur_col_value + 1);
              score.hits <- score.hits + number;
              if (cur_row_value == 4 || cur_col_value == 4) then (
                score.won <- true;
                state.code <- Some (((board_sum board) - score.hits) * number);
              ) else
                ()
        in
          List.combine state.game.boards state.scores
          |> List.iter (update_single_score state number)
    in
    match numbers with
      [] -> raise (Err "Failed to parse input")
    | number::tail_numbers ->
        update_scores number state;
        match win_check state with
            None -> inner tail_numbers state win_check
          | Some x -> x
  in
  inner state.game.numbers state

let part1 input =
  let win_check state = state.code in
  let game = build_game input in
  let state = build_state game in bingo state win_check

let part2 input =
  let win_check state =
    if List.for_all (fun score -> score.won) state.scores then
      state.code
    else
      None
  in
  let game = build_game input in
  let state = build_state game in bingo state win_check