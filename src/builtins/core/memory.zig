const std = @import("std");

const Forth = @import("../../Forth.zig");

const f_bool = Forth.f_bool;
const Cell = Forth.Cell;
const Char = Forth.Char;
const cell_size = Forth.cell_size;

pub fn @"!"(forth: *Forth) callconv(.C) noreturn {
    const addr = @as(*Cell, @ptrFromInt(forth.popu()));
    const val = forth.pop();
    addr.* = val;
    @call(.always_tail, Forth.next, .{forth});
}

pub fn @"@"(forth: *Forth) callconv(.C) noreturn {
    const addr = @as(*Cell, @ptrFromInt(forth.popu()));
    forth.push(addr.*);
    @call(.always_tail, Forth.next, .{forth});
}

pub fn @"C!"(forth: *Forth) callconv(.C) noreturn {
    const addr = @as(*Char, @ptrFromInt(forth.popu()));
    const val = @as(Char, @truncate(forth.popu()));
    addr.* = val;
    @call(.always_tail, Forth.next, .{forth});
}

pub fn @"C@"(forth: *Forth) callconv(.C) noreturn {
    const addr = @as(*Char, @ptrFromInt(forth.popu()));
    forth.push(addr.*);
    @call(.always_tail, Forth.next, .{forth});
}

pub fn ALIGN(forth: *Forth) callconv(.C) noreturn {
    forth.here = std.mem.alignPointer(forth.here, @alignOf(Cell)).?;
    @call(.always_tail, Forth.next, .{forth});
}

pub fn ALIGNED(forth: *Forth) callconv(.C) noreturn {
    const addr = forth.pop();
    forth.push(std.mem.alignForward(Cell, addr, @alignOf(Cell)));
    @call(.always_tail, Forth.next, .{forth});
}

pub fn CELLSIZE(forth: *Forth) callconv(.C) noreturn {
    forth.pushu(cell_size);
    @call(.always_tail, Forth.next, .{forth});
}

pub fn UNUSED(forth: *Forth) callconv(.C) noreturn {
    forth.pushu(
        Forth.mem_size - (@intFromPtr(forth.here) - @intFromPtr(&forth.mem)),
    );
    @call(.always_tail, Forth.next, .{forth});
}

pub fn ALLOT(forth: *Forth) callconv(.C) noreturn {
    forth.here += forth.popu();
    @call(.always_tail, Forth.next, .{forth});
}

pub fn FILL(forth: *Forth) callconv(.C) noreturn {
    const c = @as(Char, @truncate(forth.popu()));
    const len = forth.popu();
    const addr = @as([*]Char, @ptrFromInt(forth.popu()));
    @memset(addr[0..len], c);
    @call(.always_tail, Forth.next, .{forth});
}

pub fn MOVE(forth: *Forth) callconv(.C) noreturn {
    const len = forth.popu();
    const dest = @as([*]u8, @ptrFromInt(forth.popu()));
    const src = @as([*]u8, @ptrFromInt(forth.popu()));
    if (@intFromPtr(dest) <= @intFromPtr(src)) {
        std.mem.copyForwards(u8, dest[0..len], src[0..len]);
    } else {
        std.mem.copyBackwards(u8, dest[0..len], src[0..len]);
    }
    @call(.always_tail, Forth.next, .{forth});
}

/// ( x -- )
/// Reserve one cell of data space and store x in that cell. In this
/// implementation, this can be used to compile xts. This will trigger safety
/// check undefined behaviour if the data space pointer is not aligned.
pub fn @","(forth: *Forth) callconv(.C) noreturn {
    forth.compile_cell(forth.pop());
    @call(.always_tail, Forth.next, .{forth});
}

/// ( c -- )
/// Reserve one char of data space and store c in that cell.
pub fn @"C,"(forth: *Forth) callconv(.C) noreturn {
    forth.compile(@truncate(forth.popu()));
    @call(.always_tail, Forth.next, .{forth});
}

/// ( c-addr1 u1 c-addr2 u2 -- flag )
pub fn @"S="(forth: *Forth) callconv(.C) noreturn {
    const len2 = forth.popu();
    const addr2 = @as([*]u8, @ptrFromInt(forth.popu()));
    const s2 = addr2[0..len2];
    const len1 = forth.popu();
    const addr1 = @as([*]u8, @ptrFromInt(forth.popu()));
    const s1 = addr1[0..len1];
    forth.push(f_bool(std.mem.eql(u8, s1, s2)));
    @call(.always_tail, Forth.next, .{forth});
}
