open Printf

let getAutomata (filename: string) =
  let lexbuf = Lexing.from_channel (open_in filename) in
  Parser.input Lexer.main lexbuf

let buildAutomata ast explain=
  Ast.startAutomata ast explain

let playAutomataWithWord aut (word: string)=
  Ast.playAutomata aut word 

(** Usage of the CLI. *)
let usage () =
  printf "usage: ./automata [options] <filename>\n";
  printf "\n";
  printf "analysis of a grammar and application of automata.\n";
  printf "\n";
  printf "options:\n";
  printf "\t-w <word>   analyses the grammar and applies the automaton on the given word.\n";
  printf "\t-e          prints the grammar's transitions used by the automata and if he is correct.\n";
  printf "\t-p          analyses the grammar and prints it.\n";
  printf "\t-v          prints the program's version and terminates.\n";
  printf "\t-h          prints this help message and terminates.\n"

let usage_msg = "automata [options] <filename>"

let print_grammar = ref false

let input_file = ref ""

let set_input_file (filename: string) =
  input_file = filename

let speclist =
  [
    ("-p", Arg.Set print_grammar, "analyses the grammar and prints it")
  ]

(** Main function *)
let main () =
  try
    Arg.parse speclist set_input_file usage_msg;
    
  with Failure message ->
    printf "%s\n" message;
    exit 1

let () = main ()