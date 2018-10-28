# bfck

Starts a brainfuck interpreter. 

- Uses 8-bit unsigned wrapped cells
- Tape is infinite in both directions
- Outputs cleaned code if cleaning was performed
- Code is checked for valid brackets before accepting input
- Takes lines of code through STDIN until an empty line was found
- If the code contains an `!`, everything afterwards is considered input
- Otherwise takes input from STDIN until an empty line was found
- Does not work for input requiring entirely empty lines (yet!)
- Programs like `+[]` may cause infinite loops which never crash
- Throws error if there is less input than expected