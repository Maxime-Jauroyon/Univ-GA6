%{
open Ast
%}

%token INPUTSYMBOLS STACKSYMBOLS STATES INITIALSTATE INITIALSTACK TRANSITIONS PROGRAM VIRGULE EOF LPAREN RPAREN POINTVIRGULE CASE OF STATE NEXT TOP BEGIN END POP PUSH CHANGE REJECT DOUBLEPOINT
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
PROGRAM c=casesstates { Program(c) }

casesstates:
CASE STATE OF s=suitestates { CasesStates(s) }

suitestates:
l=LETTRE DOUBLEPOINT sw=suitewho s=suitestates { SuiteStates(Lettre(l),sw,s) }
| { Epsilon }

suitewho:
BEGIN CASE NEXT OF s=suiteinput END { CasesNext(s) }
| BEGIN CASE TOP OF s=suitestack END { CasesTop(s) }

suiteinput:
l=LETTRE DOUBLEPOINT sw=suitewho s=suiteinput { SuiteInputDeep(Lettre(l),sw,s) }
| l=LETTRE DOUBLEPOINT i=instruction s=suiteinput { SuiteInput(Lettre(l),i,s) }
| { EpsilonInput }

suitestack:
l=LETTRE DOUBLEPOINT sw=suitewho s=suitestack { SuiteStackDeep(Lettre(l),sw,s) }
| l=LETTRE DOUBLEPOINT i=instruction s=suitestack { SuiteStack(Lettre(l),i,s) }
| { EpsilonStack }

instruction:
POP { Pop }
| REJECT { Reject }
| PUSH l=LETTRE { Push(Lettre(l)) }
| CHANGE l=LETTRE { Change(Lettre(l)) }

lettreouvide:
 {Epsilon}
| l=LETTRE { LettreOuVide(Lettre(l))}

stack:
 {Epsilon}
| n=nonemptystack { Stack(n) }

nonemptystack:
l=LETTRE { EndStack(Lettre(l)) }
| l=LETTRE POINTVIRGULE n=nonemptystack { NonEmptyStack(Lettre(l),n)}