const std = @import("std");

const Forth = @import("Forth.zig");

const Byte = Forth.Byte;
const Cell = Forth.Cell;
const DCell = Forth.DCell;
const UCell = Forth.UCell;
const UDCell = Forth.UDCell;
const Char = Forth.Char;
const Codeword = Forth.Codeword;
const DictEntry = Forth.DictEntry;
const cell_bits = Forth.cell_bits;
const cell_size = Forth.cell_size;

const f_bool = Forth.f_bool;

pub usingnamespace @import("builtins/core.zig");
pub usingnamespace if (Forth.cfg.optional_wordsets.file)
    @import("builtins/file.zig")
else
    struct {};
