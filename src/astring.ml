open Atypes

let string_of_string_list (li: string list): string =
  let rec recursive_string_of_string_list (l: string list): string =
    match l with 
    | [] -> failwith "unexpected error"
    | [x] -> x ^ "]"
    | x::t -> x ^ "," ^ recursive_string_of_string_list t in
  if (List.length li) > 0 then "[" ^ recursive_string_of_string_list li else ""

let rec string_of_indent n = 
  match n with
  | 0 -> ""
  | x -> " " ^ string_of_indent (n - 1)

and string_of_non_nullable_char_list = function
  | NonNullableCharList(c, cl) -> c ^ ", " ^ string_of_non_nullable_char_list cl
  | NonNullableChar(c) -> c

and string_of_nullable_char = function
  | NullableChar(c) -> c
  | NullChar -> ""

and string_of_non_nullable_stack_list = function
  | NonNullableStackList(c, sl) -> c ^ ";" ^ string_of_non_nullable_stack_list sl
  | NonNullableStack(c) -> c

and string_of_nullable_stack = function
  | NullableStack(sl) -> string_of_non_nullable_stack_list sl
  | NullStack -> ""

and string_of_transition = function
  | Transition(c1,nc,c2,c3,ns) -> "(" ^ c1 ^ "," ^ string_of_nullable_char nc ^ "," ^ c2 ^ "," ^ c3 ^ "," ^ string_of_nullable_stack ns ^ ")"

and string_of_transition_list = function
  | TransitionList(t, tl) -> string_of_transition t ^ "\n" ^ string_of_transition_list tl
  | NullTransition -> ""

and string_of_instruction n f = match f with
  | Case(ca) -> "begin" ^ "\n" ^ string_of_case n ca ^ string_of_indent n ^ "end\n"
  | Change(c) -> "change " ^ c ^ "\n"
  | Push(c) -> "push " ^ c ^ "\n"
  | Pop -> "pop\n"
  | Reject -> "reject\n"

and string_of_instruction_list n f = match f with
  | InstructionList(c ,i, il) -> string_of_indent n ^ c ^ ": " ^ string_of_instruction (n + 3) i ^ string_of_instruction_list n il
  | NullInstruction -> ""

and string_of_case n f = match f with
  | State(li) -> string_of_indent n ^ "case state of\n" ^ string_of_instruction_list (n + 2) li
  | Next(li) -> string_of_indent n ^ "case next of\n" ^ string_of_instruction_list (n + 2) li
  | Top(li) -> string_of_indent n ^ "case top of\n" ^ string_of_instruction_list (n + 2) li

and string_of_algorithm = function
  | Transitions(tl) -> "\ntransitions:\n" ^ string_of_transition_list tl
  | Program(ca) -> "\nprogram:\n" ^ string_of_case 2 ca

and string_of_initial_stack = function
  | InitialStack(c) -> "initial stack: " ^ c

and string_of_initial_state = function
  | InitialState(c) -> "initial state: " ^ c

and string_of_states = function
  | States(cl) -> "states: " ^ string_of_non_nullable_char_list cl

and string_of_stack_symbols = function
  | StackSymbols(cl) -> "stack symbols: " ^ string_of_non_nullable_char_list cl

and string_of_input_symbols = function
  | InputSymbols(cl) -> "input symbols: " ^ string_of_non_nullable_char_list cl

and string_of_declarations = function
  | Declarations(i, s, st, inst, ins) -> string_of_input_symbols i ^ "\n" ^ string_of_stack_symbols s ^ "\n" ^ string_of_states st ^ "\n" ^ string_of_initial_state inst ^ "\n" ^ string_of_initial_stack ins

and string_of_automata = function
  | Automata(d, a) -> string_of_declarations d ^ "\n" ^ string_of_algorithm a
