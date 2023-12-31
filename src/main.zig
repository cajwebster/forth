const std = @import("std");

const Forth = @import("Forth.zig");
pub const forth_cfg = Forth.Cfg{
    .mem_size = 1024 * 1024,
    .stack_size = 1024,
    .max_line_len = 128,
    .optional_wordsets = .{
        .double = true,
        .file = true,
        .tools = true,
        .string = true,
    },
};

var f = Forth{};

pub fn main() !void {
    f.init();
}
