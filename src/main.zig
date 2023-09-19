const std = @import("std");

const Forth = @import("Forth.zig");
pub const forth_cfg = Forth.Cfg{
    .mem_size = 1024 * 1024,
    .stack_size = 1024,
    .max_line_len = 128,
};

var f = Forth{};

pub fn main() !void {
    f.init();
}
