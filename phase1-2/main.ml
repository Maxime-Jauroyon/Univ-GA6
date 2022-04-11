open Printf

let getAutomata (filename: string) =
  let lexbuf = Lexing.from_channel (open_in filename) in
  let ast = Parser.input Lexer.main lexbuf in
  Printf.printf "%s\n" (Ast.applyAutomata ast);
  let aut = Ast.startAutomata ast in
  aut

let playAutomataWithWord aut (word: string)=
  Ast.playAutomata aut word	

(** Usage of the CLI. *)
let usage () =
  printf "automata: analysis of a grammar and applying an automata\n";
  printf "\n";
  printf "usage:\n";
  printf "\tautomata -g FILENAME -w WORD-> analyse the grammar and apply the automata on the word\n"

(** Main function *)
let main () =
  try
    match Sys.argv with
    | [|_;"-g";file;"-w";word|] ->
      let aut = getAutomata file in
      playAutomataWithWord aut word
    | _ -> usage ()
  with Failure message ->
    printf "%s\n" message;
    exit 1

let () = main ()