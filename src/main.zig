const std = @import("std");

const Forth = @import("Forth.zig");

pub fn main() !void {
    var f: Forth = .{};
    f.init();
}
