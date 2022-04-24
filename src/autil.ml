open Atypes

let parse_automata (filename: string): automata =
  let lexbuf = Lexing.from_channel (open_in filename) in
  Aparser.automata Alexer.main lexbuf

let automata_is_interpretable (a: interpretable_automata): unit =
  let rec string_is_in_list (s: string) (l: string list): bool =
    match l with 
    | [] -> false
    | x::l' -> if String.equal s x then true else string_is_in_list s l' in

  let rec transition_is_unique (x: interpretable_transition) (l: interpretable_transition_list): bool = 
    match l with
    | [] -> true
    | y::l' -> 
      let (x_c1, x_nc, x_c2, _, _) = x in
      let (y_c1, y_nc, y_c2, _, _) = y in
      if String.equal x_c1 y_c1 && (String.equal x_nc y_nc || String.equal "" y_nc) && String.equal x_c2 y_c2 then false else transition_is_unique x l' in

  let rec transition_list_is_deterministic (l: interpretable_transition_list): bool =
    match l with
    | [] -> true
    | x::l' -> if not(transition_is_unique x l') then false else transition_list_is_deterministic l' in

  let (d, tl) = a in
  let (i, s, st, inst, ins) = d in
  if not(string_is_in_list inst st) then failwith "initial state isn't in possibles states";
  if not(string_is_in_list ins s) then failwith "initial stack isn't in possibles stack symbols";
  if tl != [] && not(transition_list_is_deterministic tl) then failwith "automata isn't deterministic"
