# gasp2

gasp2 is a pushdown automaton interpreter.

## Features

- Parses two types of file:
	- A list of automaton transitions.
	- A programming-ish coding syntax.
- Prints back a parsed file with a well formatted look.
- Checks if an automaton is deterministic.
- Interprets an automaton with a specific input.

## Compile And Run

The project uses dune as a cross-platform build system.

- Open a terminal in the project's root directory.
- Run `./automata`, the project should compile and run automatically.

## Contributors

### JAUROYON Maxime

- Initial development of each phases.
- Added the option system.
- Added more examples to demonstrate the program's capabilities.

### KINDEL Hugo

- Refactored each phases and simplified the parser.
- Separated the code in multiple files within a dune project.
- Added README.md

## License

This project is made for educational purposes only and any part of it can be used freely.
