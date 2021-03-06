type non_nullable_char_list = 
  | NonNullableCharList of string * non_nullable_char_list
  | NonNullableChar of string

type nullable_char = 
  | NullableChar of string
  | NullChar

type non_nullable_stack_list = 
  | NonNullableStackList of string * non_nullable_stack_list
  | NonNullableStack of string

type nullable_stack =
  | NullableStack of non_nullable_stack_list
  | NullStack

type transition =
  | Transition of string * nullable_char * string * string * nullable_stack

type transition_list = 
  | TransitionList of transition * transition_list
  | NullTransition

type instruction =
  | Case of case
  | Change of string
  | Push of string
  | Pop
  | Reject
and instruction_list =
  | InstructionList of string * instruction * instruction_list
  | NullInstruction
and case =
  | State of instruction_list
  | Next of instruction_list
  | Top of instruction_list

type algorithm =
  | Transitions of transition_list
  | Program of case

type initial_stack =
  | InitialStack of string

type initial_state =
  | InitialState of string

type states =
  | States of non_nullable_char_list

type stack_symbols =
  | StackSymbols of non_nullable_char_list

type input_symbols =
  | InputSymbols of non_nullable_char_list

type declarations =
  | Declarations of input_symbols * stack_symbols * states * initial_state * initial_stack

type automata =
  | Automata of declarations * algorithm

type interpretable_transition = string * string * string * string * (string list)

type interpretable_transition_list = interpretable_transition list

type interpretable_declarations = (string list) * (string list) * (string list) * string * string

type interpretable_automata = interpretable_declarations * interpretable_transition_list

type configuration = string * (string list) * string
