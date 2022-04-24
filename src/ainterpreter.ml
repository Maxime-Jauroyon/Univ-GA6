open Printf
open Atypes

let string_of_list (li: string list): string =
  let rec parcour (l: string list): string =
    match l with 
      | [] -> failwith "unexpected error"
      | [x] -> x ^ "]"
      | x::t -> x ^ "," ^ parcour t in
  if (List.length li) > 0 then "[" ^ parcour li else ""

let equalStack (st1: string list) (st2: string list): bool =
  let len1 = List.length st1 in
  let len2 = List.length st2 in
  if len1 != len2 then false else
    let rec parcour (l1: string list) (l2: string list): bool =
      match l1 with
      | [] -> 
        (match l2 with
        | [] -> true
        | x::t -> false)
      | x::t1 -> 
        (match l2 with
        | [] -> false
        | y::t2 -> if String.equal x y then parcour t1 t2 else false) in
    parcour st1 st2

let rec changeStack (st: string list) (s: string list): string list =
  match st with 
    | [] -> s
    | [x] -> s 
    | x::t -> x :: changeStack t s

let nextConfig (config: configuration) (t: interpretable_transition_list): configuration =
  let (q,st,w) = config in
  let len = List.length st in
  let lastStack = if len > 0 then List.nth st (len-1) else "" in
  let firstChar = if (String.length w) > 0 then String.make 1 (String.get w 0) else "" in
  let rec parcour (config: configuration) (currentQ: string) (lastStack: string) (firstChar: string) (t: interpretable_transition_list): configuration =
    match t with
    | [] -> config
    | x::t2 -> 
      let (l1,lv,l2,l3,s) = x in
      let (q,st,w) = config in
      if String.equal currentQ l1 && (String.equal lv firstChar || String.equal lv "") && (String.equal l2 lastStack || String.equal l2 "") then
        let newStack = if String.equal l2 "" then st @ s else changeStack st s in
        let newWord = if String.equal lv "" then w else String.sub w 1 ((String.length w)-1) in
        (l3,newStack,newWord)
      else 
        parcour config currentQ lastStack firstChar t2 in
  parcour config q lastStack firstChar t

let interpret_automata (aut: interpretable_automata) (word: string): unit =

  let (dec,tran) = aut in
  let (i,s,st,inst,ins) = dec in
  let initialConfig = (inst,[ins],word) in

  let rec play (config: configuration) (t: interpretable_transition_list): unit =
    let (q,st,w) = config in
    printf "(%s,%s,%s)\n" q (string_of_list st) w;
    let newConfig = nextConfig config t in
    let (q2,st2,w2) = newConfig in
    if String.equal q q2 && equalStack st st2 && String.equal w w2 then
      if st == [] && String.equal w "" then printf "\naccept!\n" else
        if st == [] && not(String.equal w "") then printf "empty stack without empty entry\n" else
          if st != [] && String.equal w "" then printf "empty entry without empty stack\n" else
            printf "\ncan't apply any transition\n"
    else
      if String.equal q2 "\nreject\n" then printf "\nreject!\n" else
        play newConfig t in

  play initialConfig tran
