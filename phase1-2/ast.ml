open String
open Printf
open List

type lettre = Lettre of string

type suitelettresnonvide = 
| SuiteLettres of lettre * suitelettresnonvide
| EndSuiteLettres of lettre

type lettreouvide = 
| LettreOuVide of lettre
| Epsilon

type nonemptystack = 
| NonEmptyStack of lettre * nonemptystack
| EndStack of lettre

type stack =
| Stack of nonemptystack
| Epsilon

type transition = Transition of lettre * lettreouvide * lettre * lettre * stack

type translist = 
| TransList of transition * translist
| Epsilon

type transitions = Transitions of translist

type states = States of suitelettresnonvide

type initialstate = InitialState of lettre

type initialstack = InitialStack of lettre

type stacksymbols = StackSymbols of suitelettresnonvide

type inputsymbols = InputSymbols of suitelettresnonvide

type declarations = Dec of inputsymbols * stacksymbols * states * initialstate * initialstack

type grammar = Grammar of declarations * transitions

type automata = Automata of grammar









let rec applyLettre = function
  | Lettre(x) -> x

and applyNonEmptyStack = function
  | NonEmptyStack(l,n) -> applyLettre l ^ ";" ^ applyNonEmptyStack n
  | EndStack(l) -> applyLettre l

and applyStack = function
  | Stack(n) -> applyNonEmptyStack n
  | Epsilon -> ""

and applyLettreOuVide = function
  | LettreOuVide(l) -> applyLettre l
  | Epsilon -> ""

and applyTransition = function
  | Transition(l1,lv,l2,l3,s) -> "(" ^ applyLettre l1 ^ "," ^ applyLettreOuVide lv ^ "," ^ applyLettre l2 ^ "," ^ applyLettre l3 ^ "," ^ applyStack s ^ ")"

and applyTransList = function
  | TransList(t,tl) -> applyTransition t ^ "\n" ^ applyTransList tl
  | Epsilon -> ""

and applyTransitions = function
  | Transitions(tl) -> "transitions:\n\n" ^ applyTransList tl

and applySuiteLettresNonVide = function
  | SuiteLettres(l,s) -> applyLettre l ^ ", " ^ applySuiteLettresNonVide s
  | EndSuiteLettres(l) -> applyLettre l

and applyInitialStack = function
  | InitialStack(l) -> "initial stack: " ^ applyLettre l

and applyInitialState = function
  | InitialState(l) -> "initial state: " ^ applyLettre l

and applyStates = function
  | States(s) -> "states: " ^ applySuiteLettresNonVide s

and applyStackSymbols = function
  | StackSymbols(s) -> "stack symbols: " ^ applySuiteLettresNonVide s

and applyInputSymbols = function
  | InputSymbols(s) -> "input symbols: " ^ applySuiteLettresNonVide s

and applyDec = function
  | Dec(i,s,st,inst,ins) -> applyInputSymbols i ^ "\n" ^ applyStackSymbols s ^ "\n" ^ applyStates st ^ "\n" ^ applyInitialState inst ^ "\n" ^ applyInitialStack ins

and applyGrammar = function
  | Grammar(d,t) -> applyDec d ^ "\n\n" ^ applyTransitions t

and applyAutomata = function
  | Automata(g) -> applyGrammar g
















type transitionP = string * string * string * string * (string list)
type transitionsP = transitionP list
type grammarP = (string list) * (string list) * (string list) * string * string
type automataP = grammarP * transitionsP
type configuration = string * (string list) * string



let rec buildLettre = function
  | Lettre(x) -> x

and buildNonEmptyStack = function
  | NonEmptyStack(l,n) -> buildLettre l :: buildNonEmptyStack n
  | EndStack(l) -> [buildLettre l]

and buildStack = function
  | Stack(n) -> buildNonEmptyStack n
  | Epsilon -> []

and buildLettreOuVide = function
  | LettreOuVide(l) -> buildLettre l
  | Epsilon -> ""

and buildTransition = function
  | Transition(l1,lv,l2,l3,s) -> ( buildLettre l1 , buildLettreOuVide lv , buildLettre l2 , buildLettre l3 , buildStack s )

and buildTransList = function
  | TransList(t,tl) -> buildTransition t :: buildTransList tl
  | Epsilon -> []

and buildTransitions = function
  | Transitions(tl) -> buildTransList tl

and buildSuiteLettresNonVide = function
  | SuiteLettres(l,s) -> buildLettre l :: buildSuiteLettresNonVide s
  | EndSuiteLettres(l) -> [buildLettre l]

and buildInitialStack = function
  | InitialStack(l) -> buildLettre l

and buildInitialState = function
  | InitialState(l) -> buildLettre l

and buildStates = function
  | States(s) -> buildSuiteLettresNonVide s

and buildStackSymbols = function
  | StackSymbols(s) -> buildSuiteLettresNonVide s

and buildInputSymbols = function
  | InputSymbols(s) -> buildSuiteLettresNonVide s

and buildDec = function
  | Dec(i,s,st,inst,ins) -> (buildInputSymbols i, buildStackSymbols s, buildStates st, buildInitialState inst, buildInitialStack ins)

and buildGrammar = function
  | Grammar(d,t) -> (buildDec d, buildTransitions t)

and buildAutomata = function
  | Automata(g) -> buildGrammar g














let goodAutomata (a:automataP): unit =
  let (dec,tran) = a in

  let rec isIn (s: string) (l: string list): bool =
    match l with 
    | [] -> false
    | x::t -> if equal x s then true else isIn s t in

  let (i,s,st,inst,ins) = dec in
  if not(isIn inst st) then failwith "initial state isn't in possibles states";
  if not(isIn ins s) then failwith "initial stack isn't in possibles stack symbols";



  let rec isUnique (x:transitionP) (l:transitionsP): bool = 
    match l with
    | [] -> true
    | y::t -> 
      let (xl1,xlv,xl2,xl3,xs) = x in
      let (yl1,ylv,yl2,yl3,ys) = y in
      if equal xl1 yl1 && equal xl2 yl2 && (equal xlv ylv || equal ylv "") then false else isUnique x t in

  let rec isDeterministic (l:transitionsP): bool =
    match l with
    | [] -> true
    | x::t -> if not(isUnique x t) then false else isDeterministic t in

  if tran != [] && not(isDeterministic tran) then failwith "automata isn't deterministic"



let startAutomata a: automataP = 
  let aut = buildAutomata a in
  goodAutomata aut;
  printf "everything seems fine !\n\n";
  aut
















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
        | y::t2 -> if equal x y then parcour t1 t2 else false) in
    parcour st1 st2

let rec changeStack (st: string list) (s: string list): string list =
  match st with 
    | [] -> s
    | [x] -> s 
    | x::t -> x :: changeStack t s

let nextConfig (config:configuration) (t:transitionsP): configuration =
  let (q,st,w) = config in
  let len = List.length st in
  let lastStack = if len > 0 then nth st (len-1) else "" in
  let firstChar = if (String.length w) > 0 then make 1 (get w 0) else "" in
  let rec parcour (config: configuration) (currentQ: string) (lastStack: string) (firstChar: string) (t: transitionsP): configuration =
    match t with
    | [] -> config
    | x::t2 -> 
      let (l1,lv,l2,l3,s) = x in
      let (q,st,w) = config in
      if equal currentQ l1 && (equal lv firstChar || equal lv "") && equal l2 lastStack then
        let newStack = changeStack st s in
        let newWord = if equal lv "" then w else sub w 1 ((String.length w)-1) in
        (l3,newStack,newWord)
      else 
        parcour config currentQ lastStack firstChar t2 in
  parcour config q lastStack firstChar t

let playAutomata (aut: automataP) (word: string): unit =

  let (dec,tran) = aut in
  let (i,s,st,inst,ins) = dec in
  let initialConfig = (inst,[ins],word) in

  let rec play (config: configuration) (t: transitionsP): unit =
    let (q,st,w) = config in
    printf "(%s,%s,%s)\n" q (string_of_list st) w;
    let newConfig = nextConfig config t in
    let (q2,st2,w2) = newConfig in
    if equal q q2 && equalStack st st2 && equal w w2 then
      if st == [] && equal w "" then printf "accept() !\n" else
        if st == [] && not(equal w "") then printf "empty stack without empty entry\n" else
          if st != [] && equal w "" then printf "empty entry without empty stack\n" else
            printf "can't apply any transition\n"
    else
      play newConfig t in

  play initialConfig tran



