%{
open Ast
%}

%token INPUTSYMBOLS STACKSYMBOLS STATES INITIALSTATE INITIALSTACK TRANSITIONS PROGRAM VIRGULE EOF LPAREN RPAREN POINTVIRGULE
%token<string> LETTRE
%start<Ast.automata> input

%%

  
input: a=automata EOF { Automata(a) }

automata:
d=declarations t=transitions { TransitionGrammar(d,t) }
| d=declarations p=program { ProgramGrammar(d,p) }

declarations:
i=inputsymbols s=stacksymbols st=states inst=initialstate ins=initialstack { Dec(i,s,st,inst,ins) }

inputsymbols:
INPUTSYMBOLS s=suitelettresnonvide { InputSymbols(s) }

stacksymbols:
STACKSYMBOLS s=suitelettresnonvide { StackSymbols(s) }

states:
STATES s=suitelettresnonvide { States(s) }

initialstate:
INITIALSTATE l=LETTRE {InitialState(Lettre(l)) }

initialstack:
INITIALSTACK l=LETTRE {InitialStack(Lettre(l)) }

suitelettresnonvide:
l=LETTRE { EndSuiteLettres(Lettre(l)) }
| l=LETTRE VIRGULE s=suitelettresnonvide { SuiteLettres(Lettre(l),s) }
| { failwith "unexpected empty list" }


transitions:
TRANSITIONS t=translist { Transitions(t) }

translist:
 { Epsilon }
| tr=transition t=translist { TransList(tr,t) } 

transition:
LPAREN l1=LETTRE VIRGULE lv=lettreouvide VIRGULE l2=LETTRE VIRGULE l3=LETTRE VIRGULE s=stack RPAREN { Transition(Lettre(l1),lv,Lettre(l2),Lettre(l3),s) }

program:
PROGRAM t=translist { Program(t) }

lettreouvide:
 {Epsilon}
| l=LETTRE { LettreOuVide(Lettre(l))}

stack:
 {Epsilon}
| n=nonemptystack { Stack(n) }

nonemptystack:
l=LETTRE { EndStack(Lettre(l)) }
| l=LETTRE POINTVIRGULE n=nonemptystack { NonEmptyStack(Lettre(l),n)}