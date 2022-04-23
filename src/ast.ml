open Atypes

let rec applyLettre = function
  | Lettre(x) -> x

and applyTab n = 
  match n with
  | 0 -> ""
  | x -> "  "^applyTab (n-1)

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
  | TransitionsList(t,tl) -> applyTransition t ^ "\n" ^ applyTransList tl
  | Epsilon -> ""

and applyInstruction = function
  | Pop -> "pop\n"
  | Reject -> "reject\n"
  | Push(l) -> "push " ^ applyLettre l ^ "\n"
  | Change(l) -> "change " ^ applyLettre l ^ "\n"

and applySuiteInput n f = match f with
  | SuiteInputDeep(l,sw,si) -> applyTab n ^ applyLettre l ^ ": " ^ applySuiteWho (n+1) sw ^ applySuiteInput n si
  | SuiteInput(l,i,si) -> applyTab n ^ applyLettre l ^ ": " ^ applyInstruction i ^ applySuiteInput n si
  | EpsilonInput -> ""

and applySuiteStack n f = match f with
  | SuiteStackDeep(l,sw,ss) -> applyTab n ^ applyLettre l ^ ": " ^ applySuiteWho (n+1) sw ^ applySuiteStack n ss
  | SuiteStack(l,i,ss) -> applyTab n ^ applyLettre l ^ ": " ^ applyInstruction i ^ applySuiteStack n ss
  | EpsilonStack -> ""

and applySuiteWho n f = match f with
  | CasesNext(si) -> "begin\n" ^ applyTab n ^ " case next of\n" ^ applySuiteInput (n+1) si ^ applyTab n ^ " end\n"
  | CasesTop(ss) -> "begin\n" ^ applyTab n ^ " case top of\n" ^ applySuiteStack (n+1) ss ^ applyTab n ^ " end\n"

and applySuiteStates = function
  | SuiteStates(l,sw,s) -> applyTab 2 ^ applyLettre l ^ ": " ^ applySuiteWho 3 sw ^ applySuiteStates s
  | Epsilon -> ""

and applyCasesStates = function
  | CasesStateOf(s) -> applyTab 1 ^ "case state of\n" ^ applySuiteStates s

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
  | Declarations(i,s,st,inst,ins) -> applyInputSymbols i ^ "\n" ^ applyStackSymbols s ^ "\n" ^ applyStates st ^ "\n" ^ applyInitialState inst ^ "\n" ^ applyInitialStack ins

and applyAlgorithm = function
  | Transitions(ta) -> "transitions:\n\n" ^ applyTransList ta
  | Program(ca) -> "program:\n" ^ applyCasesStates ca

and applyAutomata = function
  | Automata(d, a) -> applyDec d ^ "\n\n" ^ applyAlgorithm a
