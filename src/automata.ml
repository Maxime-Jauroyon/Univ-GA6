open Printf

let getAutomata (filename: string) =
  let lexbuf = Lexing.from_channel (open_in filename) in
  Parser.input Lexer.main lexbuf

let playAutomataWithWord ast (word: string)=
  let aut = Ast.startAutomata ast in
  Ast.playAutomata aut word	

(** Usage of the CLI. *)
let usage () =
  printf "automata: analysis of a grammar and applying an automata\n";
  printf "\n";
  printf "usage: ( -g option is mandatory) \n";
  printf "\t -g FILENAME -> analyse the grammar \n";
  printf "\t -w WORD -> analyse the grammar and apply the automata on the word\n";
  printf "\t -p -> analyse the grammar and print it\n"

(** Main function *)
let main () =
  try
    match Sys.argv with
    | [|_;"-g";file|] ->
      let _ = getAutomata file in
      Printf.printf "Correct Grammar !\n"
    | [|_;"-g";file;"-w";word|] ->
      let ast = getAutomata file in
      playAutomataWithWord ast word
    | [|_;"-g";file;"-p"|] ->
      let ast = getAutomata file in
      Printf.printf "%s\n" (Ast.applyAutomata ast)
    | [|_;"-g";file;"-w";word;"-p"|]
    | [|_;"-g";file;"-p";"-w";word|] ->
      let ast = getAutomata file in
      Printf.printf "%s\n" (Ast.applyAutomata ast);
      playAutomataWithWord ast word
    | _ -> usage ()
  with Failure message ->
    printf "%s\n" message;
    exit 1

let () = main ()