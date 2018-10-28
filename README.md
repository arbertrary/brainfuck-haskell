# brainfuck-haskell

A simple, complete brainfuck interpreter program that focusses on usability and code readability. 

- Uses 8-bit unsigned wrapped cells
- Tape is infinite in both directions
- Outputs cleaned code if verbose and when cleaning was performed 
- Code is checked for valid brackets before accepting input
- Takes lines of code through STDIN until an empty line was found
- If the code contains an `!`, everything afterwards is considered input
- Otherwise takes input from STDIN until an empty line was found
- Does not work for input requiring entirely empty lines (yet!)
- There is no way to alter initial tape state except by prepending code.
- Programs like `+[]` may cause infinite loops which never crash
- Throws error if there is less input than expected
- If verbose prints the resulting tape as well

## Build, run and test

Builiding and running requires [The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/) installed.

Run `stack build` to build, `stack run` to run the program and `stack test` to run the unit tests. 

Everything else including getting dependencies is taken care of automatically by Stack.
