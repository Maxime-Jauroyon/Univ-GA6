{
open Parser
}

let spaces = [' ' '\t' '\n']
let alphanum = ['0'-'9' 'a'-'z' 'A'-'Z']

rule main = parse
  | "input symbols" { KEYWORD_INPUT_SYMBOLS }
  | "stack symbols" { KEYWORD_STACK_SYMBOLS }
  | "states" { KEYWORD_STATES }
  | "initial state" { KEYWORD_INITIAL_STATE }
  | "initial stack" { KEYWORD_INITIAL_STACK }
  | "transitions" { KEYWORD_TRANSITIONS }
  | "program" { KEYWORD_PROGRAM }
  | "case" { KEYWORD_CASE }
  | "state" { KEYWORD_STATE }
  | "next" { KEYWORD_NEXT }
  | "top" { KEYWORD_TOP }
  | "of" { KEYWORD_OF }
  | "begin" { KEYWORD_BEGIN }
  | "end" { KEYWORD_END }
  | "pop" { KEYWORD_POP }
  | "push" { KEYWORD_PUSH }
  | "reject" { KEYWORD_REJECT }
  | "change" { KEYWORD_CHANGE }
  | '(' { PONCTUATOR_LPAREN }
  | ')' { PONCTUATOR_RPAREN }
  | ',' { PONCTUATOR_COMMA }
  | ':' { PONCTUATOR_COLON }
  | ';' { PONCTUATOR_SEMICOLON }
  | alphanum { CONSTANT_CHAR(Lexing.lexeme lexbuf) }
  | alphanum+ { failwith "lexical error" }
  | spaces { main lexbuf }
  | eof { EOF }
  | _ { failwith "unexpected character" }
