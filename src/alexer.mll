{
open Aparser
}

let alphanum = ['0'-'9' 'a'-'z' 'A'-'Z']
let spaces = [' ' '\t' '\n']

rule main = parse
  | "input"       { KEYWORD_INPUT }
  | "stack"       { KEYWORD_STACK }
  | "state"       { KEYWORD_STATE }
  | "states"      { KEYWORD_STATES }
  | "symbols"     { KEYWORD_SYMBOLS }
  | "initial"     { KEYWORD_INITIAL }
  | "transitions" { KEYWORD_TRANSITIONS }
  | "program"     { KEYWORD_PROGRAM }
  | "case"        { KEYWORD_CASE }
  | "next"        { KEYWORD_NEXT }
  | "top"         { KEYWORD_TOP }
  | "of"          { KEYWORD_OF }
  | "begin"       { KEYWORD_BEGIN }
  | "end"         { KEYWORD_END }
  | "pop"         { KEYWORD_POP }
  | "push"        { KEYWORD_PUSH }
  | "reject"      { KEYWORD_REJECT }
  | "change"      { KEYWORD_CHANGE }
  | '('           { PONCTUATOR_LPAREN }
  | ')'           { PONCTUATOR_RPAREN }
  | ','           { PONCTUATOR_COMMA }
  | ':'           { PONCTUATOR_COLON }
  | ';'           { PONCTUATOR_SEMICOLON }
  | alphanum      { CONSTANT_CHAR(Lexing.lexeme lexbuf) }
  | alphanum+     { failwith "lexical error" }
  | spaces        { main lexbuf }
  | eof           { EOF }
  | _             { failwith "unexpected character" }
