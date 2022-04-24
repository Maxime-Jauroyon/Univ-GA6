open Printf
open Atypes

let print_version = ref false
let print_grammar = ref false
let print_transitions = ref false
let input_files = ref []
let input_word = ref ""

let set_input_file filename = input_files := filename :: !input_files

let speclist = [
  ("-w", Arg.Set_string input_word, "Analyse the grammar and apply the automaton on the given word");
  ("-e", Arg.Set print_transitions, "Print the grammar's transitions used by the automaton and if it's correct");
  ("-p", Arg.Set print_grammar, "Analyse the grammar and print it");
  ("-v", Arg.Set print_version, "Display the program's version")
]

let usage_msg = "automata [options] <filename>"

let main =
  try
    Arg.parse speclist set_input_file usage_msg;
    if !print_version then
      printf "version: 1.0.0\n"
    else if List.length !input_files <> 1 then
      Arg.usage speclist usage_msg
    else
      let will_interpret_word = (String.equal !input_word "") = false in
      let aut = Autil.parse_automata (List.hd !input_files) in
      Aprint.print_automata aut !print_grammar (will_interpret_word || !print_transitions);
      let aut = Atransitions.transitions_from_automata aut in
      Autil.automata_is_interpretable aut;
      Aprint.print_interpretable_automata aut !print_transitions will_interpret_word;
      if will_interpret_word then
        Ainterpreter.interpret_automata aut !input_word
  with Failure message ->
    printf "%s\n" message;
    exit 1

let () = main
