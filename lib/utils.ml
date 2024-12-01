open Stdio

let format_day_results part_1_result part_2_result =
  "Part 1: " ^ part_1_result ^ " Part 2: " ^ part_2_result
;;

let read_file_line_by_line filename : string list =
  In_channel.with_file filename ~f:(fun in_channel -> In_channel.input_lines in_channel)
;;

let first_element_exn a =
  match a with
  | [] -> failwith "List / array is empty "
  | h :: _ -> h
;;
