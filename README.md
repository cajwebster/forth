# A Forth-2012 System written in Zig

Currently implements the entire core word set.

## TODO

- [ ] Refactoring <!-- Note to future self: This can never be checked off -->
  - [ ] Add doc comments with stack effect diagrams to every builtin
  - [ ] Add doc comments and stack effect comments to every word in forth.f
  - [ ] Limit lines to 80 characters in all files
- [X] Core words
- [X] Core extension words
- [ ] Comptime minimization of forth source files
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
- [X] File access
- [ ] Strings
  - [X] CMOVE
