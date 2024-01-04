(* PARSING *)
module Round = Map.Make (String)

type game = {
  id: int;
  rounds: int Round.t list;
}

let rec make_rounds rounds = 
  let rec process_cubes cubes round = match cubes with
  | [] -> round 
  | x :: v -> 
    let cube = String.split_on_char ' ' x in
    match List.nth cube 1 with 
    | "red" -> 
        let round = Round.add "red" (int_of_string (List.nth cube 0)) round in
        process_cubes v round
    | "green" ->
        let round = Round.add "green" (int_of_string (List.nth cube 0)) round in
        process_cubes v round
    | "blue" ->
        let round = Round.add "blue" (int_of_string (List.nth cube 0)) round in
        process_cubes v round
    | _ -> round;
  in

  match rounds with
  | [] -> []
  | x :: v ->
    let cubes = String.split_on_char ',' x
      |> List.map (fun s -> String.trim s) in
    let round = Round.empty 
      |> Round.add "red" 0 
      |> Round.add "green" 0 
      |> Round.add "blue" 0 in
    let round = process_cubes cubes round in
    round :: make_rounds v


let rec parse_lines lines = 
  let parse_line line =
    let split_game_content = String.split_on_char ':' line in
    let game_id = 
      split_game_content
      |> List.hd
      |> String.split_on_char ' '
      |> List.tl
      |> List.hd
      |> int_of_string in

    let rounds = List.nth split_game_content 1 
      |> String.split_on_char ';' 
      |> List.map (fun s -> String.trim s)
      |> make_rounds in

    { id = game_id; rounds = rounds; }
  in

  match lines with
  | [] -> []
  | x :: v -> parse_line x :: parse_lines v

(* EXECUTION *)
let filter_valid_games max_cubes games =

  let rec filter_rounds rounds = match rounds with
  | [] -> true
  | x :: v -> 
      let red = Round.find "red" x in
      let green = Round.find "green" x in
      let blue = Round.find "blue" x in

      if red > Round.find "red" max_cubes || 
        green > Round.find "green" max_cubes || 
        blue > Round.find "blue" max_cubes then false
      else filter_rounds v
  in

  let rec filter_games games = match games with
  | [] -> []
  | x :: v -> 
    if filter_rounds x.rounds then x :: filter_games v
    else filter_games v
  in

  filter_games games

let get_min_set_by_games games =
  let rec get_min_set_by_rounds rounds red green blue = match rounds with
  | [] -> 
      Round.empty 
        |> Round.add "red" red 
        |> Round.add "green" green 
        |> Round.add "blue" blue
  | x :: v ->
    let red = max red (Round.find "red" x) in
    let green = max green (Round.find "green" x) in
    let blue = max blue (Round.find "blue" x) in
    get_min_set_by_rounds v red green blue
  in

  let rec get_min_set games = match games with
  | [] -> []
  | x :: v ->
    let min_set = get_min_set_by_rounds x.rounds 0 0 0 in
    min_set :: get_min_set v
  in

  get_min_set games

let exec filename =
  let lines = Helper.read_lines filename in
  let games = parse_lines lines in

  let max_cubes = Round.empty 
    |> Round.add "red" 12
    |> Round.add "green" 13
    |> Round.add "blue" 14 in
  let valid_games = filter_valid_games max_cubes games in

  let sum = List.fold_left (fun acc x -> acc + x.id) 0 valid_games in
  print_endline ("Result: " ^ string_of_int sum)

let exec2 filename = 
  let lines = Helper.read_lines filename in
  let games = parse_lines lines in
  let min_sets = get_min_set_by_games games in
  let power = min_sets
  |> List.map (fun x -> (Round.find "red" x) * (Round.find "green" x) * (Round.find "blue" x)) in
  let sum = List.fold_left (fun acc x -> acc + x) 0 power in

  print_endline ("Result: " ^ string_of_int sum)

