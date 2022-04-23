%{
open Atypes
%}

%token KEYWORD_INPUT
%token KEYWORD_STACK
%token KEYWORD_STATE
%token KEYWORD_STATES
%token KEYWORD_SYMBOLS
%token KEYWORD_INITIAL
%token KEYWORD_TRANSITIONS
%token KEYWORD_PROGRAM
%token KEYWORD_CASE
%token KEYWORD_OF
%token KEYWORD_NEXT
%token KEYWORD_TOP
%token KEYWORD_BEGIN
%token KEYWORD_END
%token KEYWORD_POP
%token KEYWORD_PUSH
%token KEYWORD_REJECT
%token KEYWORD_CHANGE
%token PONCTUATOR_LPAREN
%token PONCTUATOR_RPAREN
%token PONCTUATOR_COMMA
%token PONCTUATOR_COLON
%token PONCTUATOR_SEMICOLON
%token<string> CONSTANT_CHAR
%token EOF

%start<Atypes.automata> automata

%%

automata:
  | d=declarations a=algorithm EOF { Automata(d, a) }

declarations:
  | i=input_symbols s=stack_symbols st=states inst=initial_state ins=initial_stack { Declarations(i, s, st, inst, ins) }

input_symbols:
  | KEYWORD_INPUT KEYWORD_SYMBOLS PONCTUATOR_COLON cl=non_nullable_char_list { InputSymbols(cl) }

stack_symbols:
  | KEYWORD_STACK KEYWORD_SYMBOLS PONCTUATOR_COLON cl=non_nullable_char_list { StackSymbols(cl) }

states:
  | KEYWORD_STATES PONCTUATOR_COLON cl=non_nullable_char_list { States(cl) }

initial_state:
  | KEYWORD_INITIAL KEYWORD_STATE PONCTUATOR_COLON c=CONSTANT_CHAR {InitialState(c) }

initial_stack:
  | KEYWORD_INITIAL KEYWORD_STACK PONCTUATOR_COLON c=CONSTANT_CHAR {InitialStack(c) }

algorithm:
  | KEYWORD_TRANSITIONS PONCTUATOR_COLON tl=transition_list { Transitions(tl) }
  | KEYWORD_PROGRAM PONCTUATOR_COLON ca=case { Program(ca) }

case:
  | KEYWORD_CASE KEYWORD_STATE KEYWORD_OF il=instruction_list { State(il) }
  | KEYWORD_CASE KEYWORD_NEXT KEYWORD_OF il=instruction_list { Next(il) }
  | KEYWORD_CASE KEYWORD_TOP KEYWORD_OF il=instruction_list { Top(il) }

instruction_list:
  | c=CONSTANT_CHAR PONCTUATOR_COLON i=instruction il=instruction_list { InstructionList(c, i, il) }
  | { Epsilon }

instruction:
  | KEYWORD_BEGIN ca=case KEYWORD_END { Case(ca) }
  | KEYWORD_CHANGE c=CONSTANT_CHAR { Change(c) }
  | KEYWORD_PUSH c=CONSTANT_CHAR { Push(c) }
  | KEYWORD_POP { Pop }
  | KEYWORD_REJECT { Reject }

transition_list:
  | t=transition tl=transition_list { TransitionList(t, tl) } 
  | { Epsilon }

transition:
  | PONCTUATOR_LPAREN c1=CONSTANT_CHAR PONCTUATOR_COMMA nc=nullable_char PONCTUATOR_COMMA c2=CONSTANT_CHAR PONCTUATOR_COMMA c3=CONSTANT_CHAR PONCTUATOR_COMMA ns=nullable_stack PONCTUATOR_RPAREN { Transition(c1, nc, c2, c3, ns) }

nullable_stack:
  | sl=non_nullable_stack_list { NullableStack(sl) }
  | { Epsilon }

non_nullable_stack_list:
  | c=CONSTANT_CHAR PONCTUATOR_SEMICOLON sl=non_nullable_stack_list { NonNullableStackList(c, sl)}
  | c=CONSTANT_CHAR { NonNullableStack(c) }

nullable_char:
  | c=CONSTANT_CHAR { NullableChar(c) }
  | { Epsilon }

non_nullable_char_list:
  | c=CONSTANT_CHAR PONCTUATOR_COMMA cl=non_nullable_char_list { NonNullableCharList(c, cl) }
  | c=CONSTANT_CHAR { NonNullableChar(c) }
  | { failwith "unexpected empty list" }
