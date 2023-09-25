# A Forth-2012 System written in Zig

Currently implements the entire core word set.

## TODO

- [ ] Refactoring <!-- Note to future self: This can never be checked off -->
  - [ ] Move functions out of builtins.zig into their own files
  - [ ] Add doc comments with stack effect diagrams to every builtin
  - [ ] Organize forth.f
  - [ ] Add doc comments and stack effect comments to every word in forth.f
  - [ ] Limit lines to 80 characters in all files
- [X] Core words
- [X] Core extension words
- [ ] Comptime minimization of src/forth.f
- [ ] Comptime configuration of VM
  - [ ] Conditional compilation of optional word sets
    - [ ] Split forth.f into multiple files
- [ ] Move I/O to seperate module
  - [ ] io.zig should throw a compile error if function signatures are incorrect
- [ ] Port to Raspberry Pi Pico
- [ ] Block access words
- [ ] Programming tools
  - [X] .S
  - [X] ?
  - [ ] DUMP
  - [ ] SEE
  - [X] WORDS
- [ ] Programming tools extensions
  - [X] BYE
- [ ] File access
  - [X] INCLUDE
  - [X] INCLUDED
  - [X] S"
- [ ] Strings
  - [X] CMOVE
