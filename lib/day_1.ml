open Base

let rec parse_input input left_list right_list =
  match input with
  | h :: t ->
    let el = String.split h ~on:' ' in
    let new_left_list = left_list @ [ Int.of_string (Utils.first_element_exn el) ] in
    let new_right_list = right_list @ [ Int.of_string (List.last_exn el) ] in
    parse_input t new_left_list new_right_list
  | [] -> left_list, right_list
;;

let part_one left_list right_list =
  let left_list_sorted = List.sort ~compare:Int.compare left_list in
  let right_list_sorted = List.sort ~compare:Int.compare right_list in
  let zipped_lists = List.zip_exn left_list_sorted right_list_sorted in
  let total_distance =
    zipped_lists
    |> List.map ~f:(fun (a, b) -> Int.abs (a - b))
    |> List.sum (module Int) ~f:Fn.id
  in
  Int.to_string total_distance
;;

let rec count_occurrences list lookup count =
  match list with
  | h :: t ->
    let new_count = if h = lookup then count + 1 else count in
    count_occurrences t lookup new_count
  | [] -> count
;;

let part_two left_list right_list =
  let res =
    List.map left_list ~f:(fun x -> count_occurrences right_list x 0 * x)
    |> List.sum (module Int) ~f:Fn.id
  in
  Int.to_string res
;;

let run_day_1 () =
  let file = Utils.read_file_line_by_line "./puzzle-inputs/day-1/input.txt" in
  let left_list, right_list = parse_input file [] [] in
  let part_1_result = part_one left_list right_list in
  let part_2_result = part_two left_list right_list in
  Utils.format_day_results part_1_result part_2_result
;;
