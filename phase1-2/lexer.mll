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
  | "," { VIRGULE }
  | ";" { POINTVIRGULE }
  | lettre { LETTRE (Lexing.lexeme lexbuf) }
  | lettre+ { failwith "lexical error" }
  | eof			{ EOF }
  | _			{ failwith "unexpected character" }
