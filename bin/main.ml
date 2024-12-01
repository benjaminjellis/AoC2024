open Base
open AoC2024

let print_day_results day result =
  Stdio.print_endline @@ "Result for day " ^ Int.to_string day ^ ": " ^ result
;;

let () =
  let day = Clap.default_int ~long:"day" ~short:'d' 1 in
  Clap.close ();
  match day with
  | 1 -> print_day_results day (Day_1.run_day_1 ())
  | _ -> failwith @@ "No solution for day: " ^ Int.to_string day
;;
