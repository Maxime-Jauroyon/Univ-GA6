{
open Parser
}

let layout = [ ' ' '\t' '\n' ]
let lettre =['0'-'9''a'-'z''A'-'Z']

rule main = parse
  | layout		{ main lexbuf }
  | ')'     { RPAREN }
  | '('     { LPAREN }
  | "input symbols:"   { INPUTSYMBOLS }
  | "stack symbols:"   { STACKSYMBOLS }
  | "states:"   { STATES }
  | "initial state:"   { INITIALSTATE }
  | "initial stack:"   { INITIALSTACK }
  | "transitions:"   { TRANSITIONS }
  | "program:"   { PROGRAM }
  | "case"   { CASE }
  | "of"   { OF }
  | "state"   { STATE }
  | "next"   { NEXT }
  | "top"   { TOP }
  | "begin"   { BEGIN }
  | "end"   { END }
  | "pop"   { POP }
  | "push"   { PUSH }
  | "reject"   { REJECT }
  | "change"   { CHANGE }
  | "," { VIRGULE }
  | ";" { POINTVIRGULE }
  | ":"   { DOUBLEPOINT }
  | lettre { LETTRE (Lexing.lexeme lexbuf) }
  | lettre+ { failwith "lexical error" }
  | eof			{ EOF }
  | _			{ failwith "unexpected character" }
