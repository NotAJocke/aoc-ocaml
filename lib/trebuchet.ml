let find_first_last_numbers line =

  let match_str_number_to_int s = match s with
    | "one" -> 1
    | "two" -> 2
    | "three" -> 3
    | "four" -> 4
    | "five" -> 5
    | "six" -> 6
    | "seven" -> 7
    | "eight" -> 8
    | "nine" -> 9
    | _ -> int_of_string s
  in

  let find_first_number_regex s =
    let regex = Str.regexp {|\(one\|two\|three\|four\|five\|six\|seven\|eight\|nine\|[1-9]\)|} in

    try
      let _ = Str.search_forward regex s 0 in
      let matched = Str.matched_string s in
      let parsed = match_str_number_to_int matched in

      Some (parsed)
    with
    | Not_found -> None
  in

  let find_last_number_regex s =
    let regex = Str.regexp {|\(one\|two\|three\|four\|five\|six\|seven\|eight\|nine\|[1-9]\)|} in

    try
      let _ = Str.search_backward regex s (String.length s) in
      let matched = Str.matched_string s in
      let parsed = match_str_number_to_int matched in

      Some (parsed)
    with
    | Not_found -> None
  in
 
  (* let first = find_first_number line 0 in *)
  let first = find_first_number_regex line in
  let last = find_last_number_regex line in

  (first, last)

let rec cumulated_sum lst =
  match lst with
  | [] -> 0 
  | x :: v -> x + cumulated_sum v

let rec process_lines lines =
  match lines with
  | [] -> []
  | x :: v -> 
      let res = find_first_last_numbers x in
      let num = match res with
        | (Some x, Some y) -> int_of_string ((string_of_int x) ^ (string_of_int y))
        | _ -> 0 in
      num :: process_lines v

let exec file = 
  let lines = Helper.read_lines file in
  let nums = process_lines lines in
  let sum = cumulated_sum nums in

  print_endline ("Result: " ^ (string_of_int sum))


(*
  * Part 1 - old code

  let rec _find_first_number line idx =
    if idx >= String.length line then None

    else match line.[idx] with
    | '0'..'9' -> Some (int_of_string (String.make 1 line.[idx]))
    | _ -> _find_first_number line (idx + 1)
  in

  let rec _find_last_number line idx =
    if idx < 0 then None
    else match line.[idx] with
    | '0'..'9' -> Some (int_of_string (String.make 1 line.[idx]))
    | _ -> _find_last_number line (idx - 1)
  in
 *)
