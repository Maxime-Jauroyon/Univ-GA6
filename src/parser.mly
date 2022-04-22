%{
open Ast
%}

%token KEYWORD_INPUT_SYMBOLS
%token KEYWORD_STACK_SYMBOLS
%token KEYWORD_STATES
%token KEYWORD_INITIAL_STATE
%token KEYWORD_INITIAL_STACK
%token KEYWORD_TRANSITIONS
%token KEYWORD_PROGRAM
%token KEYWORD_CASE
%token KEYWORD_OF
%token KEYWORD_STATE
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

%start<Ast.automata> input

%%

  
input: a=automata EOF { Automata(a) }

automata:
d=declarations t=transitions { TransitionGrammar(d,t) }
| d=declarations p=program { ProgramGrammar(d,p) }

declarations:
i=inputsymbols s=stacksymbols st=states inst=initialstate ins=initialstack { Dec(i,s,st,inst,ins) }

inputsymbols:
KEYWORD_INPUT_SYMBOLS PONCTUATOR_COLON s=suitelettresnonvide { InputSymbols(s) }

stacksymbols:
KEYWORD_STACK_SYMBOLS PONCTUATOR_COLON s=suitelettresnonvide { StackSymbols(s) }

states:
KEYWORD_STATES PONCTUATOR_COLON s=suitelettresnonvide { States(s) }

initialstate:
KEYWORD_INITIAL_STATE PONCTUATOR_COLON l=CONSTANT_CHAR {InitialState(Lettre(l)) }

initialstack:
KEYWORD_INITIAL_STACK PONCTUATOR_COLON l=CONSTANT_CHAR {InitialStack(Lettre(l)) }

transitions:
KEYWORD_TRANSITIONS PONCTUATOR_COLON t=translist { Transitions(t) }

program:
KEYWORD_PROGRAM PONCTUATOR_COLON c=casesstates { Program(c) }

suitelettresnonvide:
l=CONSTANT_CHAR { EndSuiteLettres(Lettre(l)) }
| l=CONSTANT_CHAR PONCTUATOR_COMMA s=suitelettresnonvide { SuiteLettres(Lettre(l),s) }
| { failwith "unexpected empty list" }

translist:
 { Epsilon }
| tr=transition t=translist { TransList(tr,t) } 

transition:
PONCTUATOR_LPAREN l1=CONSTANT_CHAR PONCTUATOR_COMMA lv=lettreouvide PONCTUATOR_COMMA l2=CONSTANT_CHAR PONCTUATOR_COMMA l3=CONSTANT_CHAR PONCTUATOR_COMMA s=stack PONCTUATOR_RPAREN { Transition(Lettre(l1),lv,Lettre(l2),Lettre(l3),s) }

casesstates:
KEYWORD_CASE KEYWORD_STATE KEYWORD_OF s=suitestates { CasesStates(s) }

suitestates:
l=CONSTANT_CHAR PONCTUATOR_COLON sw=suitewho s=suitestates { SuiteStates(Lettre(l),sw,s) }
| { Epsilon }

suitewho:
KEYWORD_BEGIN KEYWORD_CASE KEYWORD_NEXT KEYWORD_OF s=suiteinput KEYWORD_END { CasesNext(s) }
| KEYWORD_BEGIN KEYWORD_CASE KEYWORD_TOP KEYWORD_OF s=suitestack KEYWORD_END { CasesTop(s) }

suiteinput:
l=CONSTANT_CHAR PONCTUATOR_COLON sw=suitewho s=suiteinput { SuiteInputDeep(Lettre(l),sw,s) }
| l=CONSTANT_CHAR PONCTUATOR_COLON i=instruction s=suiteinput { SuiteInput(Lettre(l),i,s) }
| { EpsilonInput }

suitestack:
l=CONSTANT_CHAR PONCTUATOR_COLON sw=suitewho s=suitestack { SuiteStackDeep(Lettre(l),sw,s) }
| l=CONSTANT_CHAR PONCTUATOR_COLON i=instruction s=suitestack { SuiteStack(Lettre(l),i,s) }
| { EpsilonStack }

instruction:
KEYWORD_POP { Pop }
| KEYWORD_REJECT { Reject }
| KEYWORD_PUSH l=CONSTANT_CHAR { Push(Lettre(l)) }
| KEYWORD_CHANGE l=CONSTANT_CHAR { Change(Lettre(l)) }

lettreouvide:
 {Epsilon}
| l=CONSTANT_CHAR { LettreOuVide(Lettre(l))}

stack:
 {Epsilon}
| n=nonemptystack { Stack(n) }

nonemptystack:
l=CONSTANT_CHAR { EndStack(Lettre(l)) }
| l=CONSTANT_CHAR PONCTUATOR_SEMICOLON n=nonemptystack { NonEmptyStack(Lettre(l),n)}