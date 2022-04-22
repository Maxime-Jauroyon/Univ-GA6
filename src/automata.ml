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


(** Main function *)
let main () =
  try
    match Sys.argv with
    | [|_;file|] ->
      let _ = getAutomata file in
      Printf.printf "Correct Grammar !\n"
    | [|_;"-w";word;file|] ->
      let ast = getAutomata file in
      let aut = buildAutomata ast false in
      playAutomataWithWord aut word
    | [|_;"-w";word;"-e";file|]
    | [|_;"-e";"-w";word;file|] ->
      let ast = getAutomata file in
      let aut = buildAutomata ast true in
      playAutomataWithWord aut word
    | [|_;"-e";file|] ->
      let ast = getAutomata file in
      let _ = buildAutomata ast true in ()
    | [|_;"-p";"-e";file|] 
    | [|_;"-e";"-p";file|] ->
      let ast = getAutomata file in
      Printf.printf "%s\n" (Ast.applyAutomata ast);
      let _  = buildAutomata ast true in ()
    | [|_;"-p";file|] ->
      let ast = getAutomata file in
      Printf.printf "%s\n" (Ast.applyAutomata ast)
    | [|_;"-w";word;"-p";file|]
    | [|_;"-p";"-w";word;file|] ->
      let ast = getAutomata file in
      Printf.printf "%s\n" (Ast.applyAutomata ast);
      let aut = buildAutomata ast false in
      playAutomataWithWord aut word
    | [|_;"-w";word;"-p";"-e";file|]
    | [|_;"-w";word;"-e";"-p";file|]
    | [|_;"-e";"-p";"-w";word;file|]
    | [|_;"-p";"-e";"-w";word;file|] ->
      let ast = getAutomata file in
      Printf.printf "%s\n" (Ast.applyAutomata ast);
      let aut = buildAutomata ast true in
      playAutomataWithWord aut word
    | [|_;"-v";file|] -> Printf.printf "1.0.0\n"
    | [|_;"-h";file|] -> usage ()
    | _ -> usage ()
  with Failure message ->
    printf "%s\n" message;
    exit 1

let () = main ()