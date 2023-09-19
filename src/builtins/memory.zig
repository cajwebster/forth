const std = @import("std");

const Forth = @import("../Forth.zig");

const f_bool = Forth.f_bool;
const Cell = Forth.Cell;
const Char = Forth.Char;
const cell_size = Forth.cell_size;

pub fn @"!"(forth: *Forth) noreturn {
    const addr = @as(*Cell, @ptrFromInt(forth.popu()));
    const val = forth.pop();
    addr.* = val;
    forth.next();
}

pub fn @"@"(forth: *Forth) noreturn {
    const addr = @as(*Cell, @ptrFromInt(forth.popu()));
    forth.push(addr.*);
    forth.next();
}

pub fn @"C!"(forth: *Forth) noreturn {
    const addr = @as(*Char, @ptrFromInt(forth.popu()));
    const val = @as(Char, @truncate(forth.popu()));
    addr.* = val;
    forth.next();
}

pub fn @"C@"(forth: *Forth) noreturn {
    const addr = @as(*Char, @ptrFromInt(forth.popu()));
    forth.push(addr.*);
    forth.next();
}

pub fn ALIGN(forth: *Forth) noreturn {
    forth.here = std.mem.alignPointer(forth.here, @alignOf(Cell)).?;
    forth.next();
}

pub fn ALIGNED(forth: *Forth) noreturn {
    const addr = forth.pop();
    forth.push(std.mem.alignForward(Cell, addr, @alignOf(Cell)));
    forth.next();
}

pub fn CELLSIZE(forth: *Forth) noreturn {
    forth.pushu(cell_size);
    forth.next();
}

pub fn UNUSED(forth: *Forth) noreturn {
    forth.pushu(Forth.mem_size - (@intFromPtr(forth.here) - @intFromPtr(&forth.mem)));
    forth.next();
}

pub fn ALLOT(forth: *Forth) noreturn {
    forth.here += forth.popu();
    forth.next();
}

pub fn FILL(forth: *Forth) noreturn {
    const c = @as(Char, @truncate(forth.popu()));
    const len = forth.popu();
    const addr = @as([*]Char, @ptrFromInt(forth.popu()));
    @memset(addr[0..len], c);
    forth.next();
}

pub fn MOVE(forth: *Forth) noreturn {
    const len = forth.popu();
    const dest = @as([*]u8, @ptrFromInt(forth.popu()));
    const src = @as([*]u8, @ptrFromInt(forth.popu()));
    if (@intFromPtr(dest) <= @intFromPtr(src)) {
        std.mem.copyForwards(u8, dest[0..len], src[0..len]);
    } else {
        std.mem.copyBackwards(u8, dest[0..len], src[0..len]);
    }
    forth.next();
}

pub fn COMPARE(forth: *Forth) noreturn {
    const len2 = forth.popu();
    const addr2 = @as([*]u8, @ptrFromInt(forth.popu()));
    const s2 = addr2[0..len2];
    const len1 = forth.popu();
    const addr1 = @as([*]u8, @ptrFromInt(forth.popu()));
    const s1 = addr1[0..len1];
    forth.push(f_bool(std.mem.eql(u8, s1, s2)));
    forth.next();
}
