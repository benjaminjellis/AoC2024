open Base

let rec parse_input file (output_list : int list list) =
  match file with
  | h :: t ->
    let elements = String.split h ~on:' ' |> List.map ~f:(fun x -> Int.of_string x) in
    parse_input t (elements :: output_list)
  | [] -> output_list
;;

let differences lst =
  match lst with
  | [] | [ _ ] -> []
  | hd :: tl ->
    let rec aux prev acc = function
      | [] -> List.rev acc
      | x :: xs -> aux x ((x - prev) :: acc) xs
    in
    aux hd [] tl
;;

let all_same_sign lst =
  let first_el = List.hd_exn lst in
  if first_el > 0
  then List.for_all lst ~f:(fun x -> x > 0)
  else List.for_all lst ~f:(fun x -> x < 0)
;;

let generate_all_combi lst =
  let rec aux prefix suffix acc =
    match suffix with
    | [] -> acc
    | x :: xs -> aux (x :: prefix) xs (List.rev_append prefix xs :: acc)
  in
  aux [] lst []
;;

let is_safe line =
  let differences = differences line in
  let same = all_same_sign differences in
  let max =
    List.map differences ~f:(fun x -> Int.abs x) |> List.max_elt ~compare:Int.compare
  in
  match max with
  | None -> failwith "fuck"
  | Some max -> if max <= 3 && same then true else false
;;

let part_2_internal lst =
  let sum =
    generate_all_combi lst
    |> List.map ~f:(fun x -> if is_safe x then 1 else 0)
    |> List.sum (module Int) ~f:Fn.id
  in
  if sum > 0 then 1 else 0
;;

let part_2 lines =
  List.map lines ~f:(fun x -> part_2_internal x)
  |> List.sum (module Int) ~f:Fn.id
  |> Int.to_string
;;

let part_1 lines =
  List.map lines ~f:(fun x -> if is_safe x then 1 else 0)
  |> List.sum (module Int) ~f:Fn.id
  |> Int.to_string
;;

let run_day_2 () =
  let file = Utils.read_file_line_by_line "./puzzle-inputs/day-2/input.txt" in
  let input = parse_input file [] in
  let part_1_result = part_1 input in
  let part_2_result = part_2 input in
  Utils.format_day_results part_1_result part_2_result
;;
