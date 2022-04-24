open Printf
open Atypes

let print_version = ref false
let print_grammar = ref false
let print_transitions = ref false
let input_file = ref []
let input_word = ref ""

let set_input_file filename = input_file := filename :: !input_file

let speclist = [
  ("-w", Arg.Set_string input_word, "Analyse the grammar and apply the automaton on the given word");
  ("-e", Arg.Set print_transitions, "Print the grammar's transitions used by the automata and if it's correct");
  ("-p", Arg.Set print_grammar, "Analyse the grammar and print it");
  ("-v", Arg.Set print_version, "Display the program's version")
]

let usage_msg = "automata [options] <filename>"

let main =
  try
    Arg.parse speclist set_input_file usage_msg;
    if !print_version then
      printf "version: 1.0.0\n"
    else if List.length !input_file <> 1 then
      Arg.usage speclist usage_msg
    else
      let aut = Autil.parse_automata (List.hd !input_file) in
      Aprint.print_automata aut !print_grammar;
      let aut = Atransitions.transitions_from_automata aut in
      Autil.automata_is_interpretable aut;
      Aprint.print_interpretable_automata aut !print_transitions;
      if (String.equal !input_word "") = false then
        Ainterpreter.interpret_automata aut !input_word
  with Failure message ->
    printf "%s\n" message;
    exit 1

let () = main
