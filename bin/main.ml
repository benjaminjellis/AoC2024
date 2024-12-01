let () =
  let day = Clap.default_int ~long:"day" ~short:'d' 1 in
  Clap.close ();
  match day with
  | 1 -> ()
  | _ -> failwith "No solution for day"
;;
