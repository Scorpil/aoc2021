let usage_msg = "cat <inpit> | aoc2021 -d <int>"
let day = ref 0

let speclist =
  [("-d", Arg.Set_int day, "Number of the challange to execute")]

let maybe_read_line () =
    try Some(read_line())
    with End_of_file -> None

let rec stdin_lines acc =
  match maybe_read_line () with
  | Some(line) -> stdin_lines (line :: acc)
  | None -> acc

let input = stdin_lines [] |> List.rev

let solution day part1 part2 =
  Printf.printf "=== Day: %d === \n" day;
  Printf.printf "Part 1: %d" (part1 input);
  print_newline();
  Printf.printf "Part 2: %d" (part2 input);
  print_newline()

let () =
  Arg.parse speclist (fun (_) -> ()) usage_msg;
  match !day with
      1 -> (solution !day Day1.part1 Day1.part2)
    | 2 -> (solution !day Day2.part1 Day2.part2)
    | 3 -> (solution !day Day3.part1 Day3.part2)
    | 4 -> (solution !day Day4.part1 Day4.part2)
    | 5 -> (solution !day Day5.part1 Day5.part2)
    | 6 -> (solution !day Day6.part1 Day6.part2)
    | 7 -> (solution !day Day7.part1 Day7.part2)
    | 8 -> (solution !day Day8.part1 Day8.part2)
    | 9 -> (solution !day Day9.part1 Day9.part2)
    | 10 -> (solution !day Day10.part1 Day10.part2)
    | 11 -> (solution !day Day11.part1 Day11.part2)
    | _ -> Printf.printf "Day %d not found\n" !day
  ;;


