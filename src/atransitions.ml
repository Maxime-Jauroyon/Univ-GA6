open Atypes

let rec transitions_from_non_nullable_char_list = function
  | NonNullableCharList(c, cl) -> c :: transitions_from_non_nullable_char_list cl
  | NonNullableChar(c) -> [c]

and transitions_from_nullable_char = function
  | NullableChar(c) -> c
  | NullChar -> ""

and transitions_from_non_nullable_stack_list = function
  | NonNullableStackList(c, sl) -> c :: transitions_from_non_nullable_stack_list sl
  | NonNullableStack(c) -> [c]

and transitions_from_nullable_stack = function
  | NullableStack(sl) -> transitions_from_non_nullable_stack_list sl
  | NullStack -> []

and transitions_from_transition = function
  | Transition(c1, nc, c2, c3, ns) -> (c1, transitions_from_nullable_char nc, c2, c3, transitions_from_nullable_stack ns)

and transitions_from_transition_list = function
  | TransitionList(t, tl) -> transitions_from_transition t :: transitions_from_transition_list tl
  | NullTransition -> []

and transitions_from_instruction nc1 nc2 nc3 nc4 ns f = match f with
  | Case(ca) -> transitions_from_case nc1 nc2 nc3 nc4 ns ca
  | Change(c) -> [transitions_from_transition (Transition (transitions_from_nullable_char nc1, nc2, transitions_from_nullable_char nc3, c, NullStack))]
  | Push(c) -> [transitions_from_transition (Transition (transitions_from_nullable_char nc1, nc2, transitions_from_nullable_char nc3, transitions_from_nullable_char nc4, NullableStack (NonNullableStack c)))]
  | Pop ->
    if ns = false then
      failwith "pop without knowing the symbol on top!"
    else
      [transitions_from_transition (Transition (transitions_from_nullable_char nc1, nc2, transitions_from_nullable_char nc3, transitions_from_nullable_char nc4, NullStack))]
  | Reject -> [transitions_from_transition (Transition (transitions_from_nullable_char nc1, nc2, transitions_from_nullable_char nc3, "reject", NullStack))]

and transitions_from_instruction_list nc1 nc2 nc3 nc4 ns f p = match f with
  | InstructionList(c ,i, il) -> (match p with
    | State(_) -> transitions_from_instruction (NullableChar c) nc2 nc3 (NullableChar c) ns i
    | Next(_) -> transitions_from_instruction nc1 (NullableChar c) nc3 nc4 ns i
    | Top(_) -> transitions_from_instruction nc1 nc2 (NullableChar c) nc4 true i) @ transitions_from_instruction_list nc1 nc2 nc3 nc4 ns il p
  | NullInstruction -> []

and transitions_from_case nc1 nc2 nc3 nc4 ns f = match f with
  | State(li) -> transitions_from_instruction_list nc1 nc2 nc3 nc4 ns li f
  | Next(li) -> transitions_from_instruction_list nc1 nc2 nc3 nc4 ns li f
  | Top(li) -> transitions_from_instruction_list nc1 nc2 nc3 nc4 ns li f

and transitions_from_algorithm f = match f with
  | Transitions(tl) -> transitions_from_transition_list tl
  | Program(ca) -> transitions_from_case NullChar NullChar NullChar NullChar false ca

and transitions_from_initial_stack = function
  | InitialStack(c) -> c

and transitions_from_initial_state = function
  | InitialState(c) -> c

and transitions_from_states = function
  | States(cl) -> transitions_from_non_nullable_char_list cl

and transitions_from_stack_symbols = function
  | StackSymbols(cl) -> transitions_from_non_nullable_char_list cl

and transitions_from_input_symbols = function
  | InputSymbols(cl) -> transitions_from_non_nullable_char_list cl

and transitions_from_declarations = function
  | Declarations(i, s, st, inst, ins) -> (transitions_from_input_symbols i, transitions_from_stack_symbols s, transitions_from_states st, transitions_from_initial_state inst, transitions_from_initial_stack ins)

and transitions_from_automata = function
  | Automata(d, a) -> (transitions_from_declarations d, transitions_from_algorithm a)
