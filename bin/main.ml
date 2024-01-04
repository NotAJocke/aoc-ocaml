open Oac_ocaml

let () = 
  let multiline_string = {|
  1: Trebuchet, part 1
  2: Trebuchet, part 2
  3: Cube Conundrum, part 1
  4: Cube Conundrum, part 2

  't': Run tests
  |} in

  Helper.print_title ();
  print_endline "--------------------------------------------------\n";
  print_endline multiline_string;
  print_endline "Enter the input to run the corresponding program.";


  let input = read_line () in

  print_endline "--------------------------------------------------\n";

  match input with
  | "1" -> Trebuchet.exec "inputs/trebuchet1.txt"
  | "2" -> Trebuchet.exec "inputs/trebuchet2.txt"
  | "3" -> Cube.exec "inputs/cube.txt"
  | "4" -> Cube.exec2 "inputs/cube.txt"
  | "t" -> 
      ()
  | _ -> print_endline "Invalid input."
