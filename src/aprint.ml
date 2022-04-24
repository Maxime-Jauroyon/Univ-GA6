open Printf
open Atypes

let print_automata (a: automata) (print: bool): unit =
  if print then printf "%s" (Astring.string_of_automata a)

let print_interpretable_automata (a: interpretable_automata) (print: bool): unit =
  let string_of_interpretable_stack_list (sl: string list): string =
    let rec recursive_string_of_interpretable_stack_list (sl: string list): string =
      match sl with 
        | [] -> failwith "unexpected error"
        | [x] -> x
        | x::l' -> x ^ ";" ^ recursive_string_of_interpretable_stack_list l' in

    if (List.length sl) > 0 then recursive_string_of_interpretable_stack_list sl else "" in

  let rec print_interpretable_transition_list tl = match tl with
      | [] -> ()
      | x::l' -> let (c1, nc, c2, c3, ns) = x in
        printf "(%s,%s,%s,%s,%s)\n" c1 nc c2 c3 (string_of_interpretable_stack_list ns);
        print_interpretable_transition_list l' in

  if print then
    let (_, tl) = a in
    printf "\ntransitions used by automata: \n\n";
    print_interpretable_transition_list tl;
    printf "\nautomata is deterministic!\n\n"
