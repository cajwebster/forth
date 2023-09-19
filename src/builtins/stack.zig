const std = @import("std");

const Forth = @import("../Forth.zig");

const Cell = Forth.Cell;

pub fn DEPTH(forth: *Forth) noreturn {
    forth.pushu(@divExact(@intFromPtr(forth.sp) - @intFromPtr(&forth.stack[0]), Forth.cell_size));
    forth.next();
}

pub fn DROP(forth: *Forth) noreturn {
    _ = forth.pop();
    forth.next();
}

pub fn DUP(forth: *Forth) noreturn {
    const n = forth.pop();
    forth.push(n);
    forth.push(n);
    forth.next();
}

pub fn @"?DUP"(forth: *Forth) noreturn {
    const n = forth.pop();
    forth.push(n);
    if (n != 0) forth.push(n);
    forth.next();
}

pub fn NIP(forth: *Forth) noreturn {
    const n = forth.pop();
    _ = forth.pop();
    forth.push(n);
    forth.next();
}

pub fn OVER(forth: *Forth) noreturn {
    const n2 = forth.pop();
    const n1 = forth.pop();
    forth.push(n1);
    forth.push(n2);
    forth.push(n1);
    forth.next();
}

pub fn PICK(forth: *Forth) noreturn {
    const n = forth.popu() + 1;
    forth.push((forth.sp - n)[0]);
    forth.next();
}

pub fn ROT(forth: *Forth) noreturn {
    const n3 = forth.pop();
    const n2 = forth.pop();
    const n1 = forth.pop();
    forth.push(n2);
    forth.push(n3);
    forth.push(n1);
    forth.next();
}

pub fn ROLL(forth: *Forth) noreturn {
    const u = forth.popu() + 1;
    if (@intFromPtr(forth.sp - u) < @intFromPtr(&forth.stack)) forth.die("stack underflow");
    if (u > 1)
        std.mem.rotate(Cell, (forth.sp - u)[0..u], 1);
    forth.next();
}

pub fn SWAP(forth: *Forth) noreturn {
    const n2 = forth.pop();
    const n1 = forth.pop();
    forth.push(n2);
    forth.push(n1);
    forth.next();
}

pub fn TUCK(forth: *Forth) noreturn {
    const n2 = forth.pop();
    const n1 = forth.pop();
    forth.push(n2);
    forth.push(n1);
    forth.push(n2);
    forth.next();
}

pub fn @"2SWAP"(forth: *Forth) noreturn {
    const n2 = forth.popd();
    const n1 = forth.popd();
    forth.pushd(n2);
    forth.pushd(n1);
    forth.next();
}

pub fn @"2DUP"(forth: *Forth) noreturn {
    const n = forth.popd();
    forth.pushd(n);
    forth.pushd(n);
    forth.next();
}

pub fn @">R"(forth: *Forth) noreturn {
    forth.rpush(forth.popu());
    forth.next();
}

pub fn @"R>"(forth: *Forth) noreturn {
    forth.pushu(forth.rpop());
    forth.next();
}

pub fn @"R@"(forth: *Forth) noreturn {
    const n = forth.rpop();
    forth.rpush(n);
    forth.pushu(n);
    forth.next();
}

pub fn @"2>R"(forth: *Forth) noreturn {
    const n2 = forth.popu();
    const n1 = forth.popu();
    forth.rpush(n1);
    forth.rpush(n2);
    forth.next();
}

pub fn @"2R>"(forth: *Forth) noreturn {
    const n2 = forth.rpop();
    const n1 = forth.rpop();
    forth.pushu(n1);
    forth.pushu(n2);
    forth.next();
}

pub fn @"2R@"(forth: *Forth) noreturn {
    const n2 = forth.rpop();
    const n1 = forth.rpop();
    forth.rpush(n1);
    forth.rpush(n2);
    forth.pushu(n1);
    forth.pushu(n2);
    forth.next();
}

pub fn @">L"(forth: *Forth) noreturn {
    forth.lpush(forth.popu());
    forth.next();
}

pub fn @"L>"(forth: *Forth) noreturn {
    forth.pushu(forth.lpop());
    forth.next();
}
