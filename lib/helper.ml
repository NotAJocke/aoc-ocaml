let read_lines name : string list =
  let ic = open_in name in

  let try_read () =
    try Some (input_line ic) with End_of_file -> None in

  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in

  loop []

let print_title () =
  let title = {|
    _       _                 _      ___   __    ____          _      
   / \   __| |_   _____ _ __ | |_   / _ \ / _|  / ___|___   __| | ___ 
  / _ \ / _` \ \ / / _ \ '_ \| __| | | | | |_  | |   / _ \ / _` |/ _ \
 / ___ \ (_| |\ V /  __/ | | | |_  | |_| |  _| | |__| (_) | (_| |  __/
/_/   \_\__,_| \_/ \___|_| |_|\__|  \___/|_|    \____\___/ \__,_|\___|  
    |}in

  print_endline title
