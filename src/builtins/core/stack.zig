const std = @import("std");

const Forth = @import("../../Forth.zig");

const Cell = Forth.Cell;

pub fn DEPTH(forth: *Forth) callconv(.C) noreturn {
    forth.pushu(@divExact(
        @intFromPtr(forth.sp) - @intFromPtr(&forth.stack[0]),
        Forth.cell_size,
    ));
    @call(.always_tail, Forth.next, .{forth});
}

pub fn DROP(forth: *Forth) callconv(.C) noreturn {
    _ = forth.pop();
    @call(.always_tail, Forth.next, .{forth});
}

pub fn DUP(forth: *Forth) callconv(.C) noreturn {
    const n = forth.pop();
    forth.push(n);
    forth.push(n);
    @call(.always_tail, Forth.next, .{forth});
}

pub fn @"?DUP"(forth: *Forth) callconv(.C) noreturn {
    const n = forth.pop();
    forth.push(n);
    if (n != 0) forth.push(n);
    @call(.always_tail, Forth.next, .{forth});
}

pub fn NIP(forth: *Forth) callconv(.C) noreturn {
    const n = forth.pop();
    _ = forth.pop();
    forth.push(n);
    @call(.always_tail, Forth.next, .{forth});
}

pub fn OVER(forth: *Forth) callconv(.C) noreturn {
    const n2 = forth.pop();
    const n1 = forth.pop();
    forth.push(n1);
    forth.push(n2);
    forth.push(n1);
    @call(.always_tail, Forth.next, .{forth});
}

pub fn PICK(forth: *Forth) callconv(.C) noreturn {
    const n = forth.popu() + 1;
    forth.push((forth.sp - n)[0]);
    @call(.always_tail, Forth.next, .{forth});
}

pub fn ROT(forth: *Forth) callconv(.C) noreturn {
    const n3 = forth.pop();
    const n2 = forth.pop();
    const n1 = forth.pop();
    forth.push(n2);
    forth.push(n3);
    forth.push(n1);
    @call(.always_tail, Forth.next, .{forth});
}

pub fn ROLL(forth: *Forth) callconv(.C) noreturn {
    const u = forth.popu() + 1;
    if (@intFromPtr(forth.sp - u) < @intFromPtr(&forth.stack))
        forth.die("stack underflow");
    if (u > 1)
        std.mem.rotate(Cell, (forth.sp - u)[0..u], 1);
    @call(.always_tail, Forth.next, .{forth});
}

pub fn SWAP(forth: *Forth) callconv(.C) noreturn {
    const n2 = forth.pop();
    const n1 = forth.pop();
    forth.push(n2);
    forth.push(n1);
    @call(.always_tail, Forth.next, .{forth});
}

pub fn TUCK(forth: *Forth) callconv(.C) noreturn {
    const n2 = forth.pop();
    const n1 = forth.pop();
    forth.push(n2);
    forth.push(n1);
    forth.push(n2);
    @call(.always_tail, Forth.next, .{forth});
}

pub fn @"2SWAP"(forth: *Forth) callconv(.C) noreturn {
    const n2 = forth.popd();
    const n1 = forth.popd();
    forth.pushd(n2);
    forth.pushd(n1);
    @call(.always_tail, Forth.next, .{forth});
}

pub fn @"2DUP"(forth: *Forth) callconv(.C) noreturn {
    const n = forth.popd();
    forth.pushd(n);
    forth.pushd(n);
    @call(.always_tail, Forth.next, .{forth});
}

pub fn @">R"(forth: *Forth) callconv(.C) noreturn {
    forth.rpush(forth.popu());
    @call(.always_tail, Forth.next, .{forth});
}

pub fn @"R>"(forth: *Forth) callconv(.C) noreturn {
    forth.pushu(forth.rpop());
    @call(.always_tail, Forth.next, .{forth});
}

pub fn @"R@"(forth: *Forth) callconv(.C) noreturn {
    const n = forth.rpop();
    forth.rpush(n);
    forth.pushu(n);
    @call(.always_tail, Forth.next, .{forth});
}

pub fn @"2>R"(forth: *Forth) callconv(.C) noreturn {
    const n2 = forth.popu();
    const n1 = forth.popu();
    forth.rpush(n1);
    forth.rpush(n2);
    @call(.always_tail, Forth.next, .{forth});
}

pub fn @"2R>"(forth: *Forth) callconv(.C) noreturn {
    const n2 = forth.rpop();
    const n1 = forth.rpop();
    forth.pushu(n1);
    forth.pushu(n2);
    @call(.always_tail, Forth.next, .{forth});
}

pub fn @"2R@"(forth: *Forth) callconv(.C) noreturn {
    const n2 = forth.rpop();
    const n1 = forth.rpop();
    forth.rpush(n1);
    forth.rpush(n2);
    forth.pushu(n1);
    forth.pushu(n2);
    @call(.always_tail, Forth.next, .{forth});
}

pub fn @">L"(forth: *Forth) callconv(.C) noreturn {
    forth.lpush(forth.popu());
    @call(.always_tail, Forth.next, .{forth});
}

pub fn @"L>"(forth: *Forth) callconv(.C) noreturn {
    forth.pushu(forth.lpop());
    @call(.always_tail, Forth.next, .{forth});
}
