open Printf
open Atypes

let equal_stack (st1: string list) (st2: string list): bool =
  let rec recursive_equal_stack (l1: string list) (l2: string list): bool =
    match l1 with
    | [] -> 
      (match l2 with
      | [] -> true
      | x::l2' -> false)
    | x::l1' -> 
      (match l2 with
      | [] -> false
      | y::l2' -> if String.equal x y then recursive_equal_stack l1' l2' else false) in

  let (len1, len2) = (List.length st1, List.length st2) in
  if len1 != len2 then false else
    recursive_equal_stack st1 st2

let rec change_stack (st: string list) (s: string list): string list =
  match st with 
    | [] -> s
    | [x] -> s 
    | x::t -> x :: change_stack t s

let get_next_config (config: configuration) (t: interpretable_transition_list): configuration =
  let rec recursive_get_next_config (config: configuration) (currentQ: string) (lastStack: string) (firstChar: string) (t: interpretable_transition_list): configuration =
    match t with
    | [] -> config
    | x::t2 -> 
      let (l1,lv,l2,l3,s) = x in
      let (q,st,w) = config in
      if String.equal currentQ l1 && (String.equal lv firstChar || String.equal lv "") && (String.equal l2 lastStack || String.equal l2 "") then
        let newStack = if String.equal l2 "" then st @ s else change_stack st s in
        let newWord = if String.equal lv "" then w else String.sub w 1 ((String.length w)-1) in
        (l3,newStack,newWord)
      else 
        recursive_get_next_config config currentQ lastStack firstChar t2 in

  let (q, st, w) = config in
  let len = List.length st in
  let lastStack = if len > 0 then List.nth st (len-1) else "" in
  let firstChar = if (String.length w) > 0 then String.make 1 (String.get w 0) else "" in
  recursive_get_next_config config q lastStack firstChar t

let interpret_automata (a: interpretable_automata) (w: string): unit =
  let rec interpret_transitions_with_configuration (config: configuration) (t: interpretable_transition_list): unit =
    let (q, st, w) = config in
    printf "(%s,%s,%s)\n" q (Astring.string_of_string_list st) w;
    let config = get_next_config config t in
    let (q2, st2, w2) = config in
    if String.equal q q2 && equal_stack st st2 && String.equal w w2 then
      if st == [] && String.equal w "" then
        printf "\naccept!\n"
      else
        if st == [] && not(String.equal w "") then
          printf "empty stack without empty entry\n"
        else
          if st != [] && String.equal w "" then
            printf "empty entry without empty stack\n"
          else
            printf "\ncan't apply any transition\n"
    else
      if String.equal q2 "\nreject\n" then
        printf "\nreject!\n"
      else
        interpret_transitions_with_configuration config t in

  let (d, tl) = a in
  let (i, s, st, inst, ins) = d in
  let config = (inst, [ins], w) in
  interpret_transitions_with_configuration config tl
