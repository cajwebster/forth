const std = @import("std");

const Forth = @import("../Forth.zig");

const Cell = Forth.Cell;
const DCell = Forth.DCell;
const UCell = Forth.UCell;
const f_bool = Forth.f_bool;

pub fn @"+"(forth: *Forth) noreturn {
    const n2 = forth.pop();
    const n1 = forth.pop();
    forth.push(n1 +% n2);
    forth.next();
}

pub fn @"-"(forth: *Forth) noreturn {
    const n2 = forth.pop();
    const n1 = forth.pop();
    forth.push(n1 -% n2);
    forth.next();
}

pub fn @"*"(forth: *Forth) noreturn {
    const n2 = forth.pop();
    const n1 = forth.pop();
    forth.push(n1 *% n2);
    forth.next();
}

pub fn @"/"(forth: *Forth) noreturn {
    const n2 = forth.pop();
    const n1 = forth.pop();
    forth.push(@divTrunc(n1, n2));
    forth.next();
}

pub fn NEGATE(forth: *Forth) noreturn {
    const n = forth.pop();
    forth.push(0 -% n);
    forth.next();
}

pub fn MOD(forth: *Forth) noreturn {
    const n2 = forth.pop();
    const n1 = forth.pop();
    forth.push(@rem(n1, n2));
    forth.next();
}

pub fn @"/MOD"(forth: *Forth) noreturn {
    const n2 = forth.pop();
    const n1 = forth.pop();
    forth.push(@rem(n1, n2));
    forth.push(@divTrunc(n1, n2));
    forth.next();
}

pub fn SQRT(forth: *Forth) noreturn {
    const n = forth.popu();
    forth.pushu(std.math.sqrt(n));
    forth.next();
}

pub fn @"="(forth: *Forth) noreturn {
    const n2 = forth.pop();
    const n1 = forth.pop();
    forth.push(f_bool(n1 == n2));
    forth.next();
}

pub fn @"<"(forth: *Forth) noreturn {
    const n2 = forth.pop();
    const n1 = forth.pop();
    forth.push(f_bool(n1 < n2));
    forth.next();
}

pub fn @">"(forth: *Forth) noreturn {
    const n2 = forth.pop();
    const n1 = forth.pop();
    forth.push(f_bool(n1 > n2));
    forth.next();
}

pub fn @"<>"(forth: *Forth) noreturn {
    const n2 = forth.pop();
    const n1 = forth.pop();
    forth.push(f_bool(n1 != n2));
    forth.next();
}

pub fn @"<="(forth: *Forth) noreturn {
    const n2 = forth.pop();
    const n1 = forth.pop();
    forth.push(f_bool(n1 <= n2));
    forth.next();
}

pub fn @">="(forth: *Forth) noreturn {
    const n2 = forth.pop();
    const n1 = forth.pop();
    forth.push(f_bool(n1 >= n2));
    forth.next();
}

pub fn @"0="(forth: *Forth) noreturn {
    const n1 = forth.pop();
    forth.push(f_bool(n1 == 0));
    forth.next();
}

pub fn @"0<"(forth: *Forth) noreturn {
    const n1 = forth.pop();
    forth.push(f_bool(n1 < 0));
    forth.next();
}

pub fn @"0>"(forth: *Forth) noreturn {
    const n1 = forth.pop();
    forth.push(f_bool(n1 > 0));
    forth.next();
}

pub fn @"0<>"(forth: *Forth) noreturn {
    const n1 = forth.pop();
    forth.push(f_bool(n1 != 0));
    forth.next();
}

pub fn @"0<="(forth: *Forth) noreturn {
    const n1 = forth.pop();
    forth.push(f_bool(n1 <= 0));
    forth.next();
}

pub fn @"0>="(forth: *Forth) noreturn {
    const n1 = forth.pop();
    forth.push(f_bool(n1 >= 0));
    forth.next();
}

pub fn @"U>"(forth: *Forth) noreturn {
    const n2 = forth.popu();
    const n1 = forth.popu();
    forth.push(f_bool(n1 > n2));
    forth.next();
}

pub fn AND(forth: *Forth) noreturn {
    const n2 = forth.pop();
    const n1 = forth.pop();
    forth.push(n1 & n2);
    forth.next();
}

pub fn OR(forth: *Forth) noreturn {
    const n2 = forth.pop();
    const n1 = forth.pop();
    forth.push(n1 | n2);
    forth.next();
}

pub fn XOR(forth: *Forth) noreturn {
    const n2 = forth.pop();
    const n1 = forth.pop();
    forth.push(n1 ^ n2);
    forth.next();
}

pub fn INVERT(forth: *Forth) noreturn {
    const n = forth.pop();
    forth.push(~n);
    forth.next();
}

pub fn @"UD/MOD"(forth: *Forth) noreturn {
    const n2 = forth.popu();
    const n1 = forth.popud();
    const q = n1 / n2;
    const r = n1 % n2;
    forth.pushud(r);
    forth.pushud(q);
    forth.next();
}

pub fn @"D0="(forth: *Forth) noreturn {
    const n = forth.popd();
    forth.push(f_bool(n == 0));
    forth.next();
}

pub fn @"M*"(forth: *Forth) noreturn {
    const n2 = forth.pop();
    const n1 = forth.pop();
    forth.pushd(std.math.mulWide(Cell, n1, n2));
    forth.next();
}

pub fn @"UM*"(forth: *Forth) noreturn {
    const n2 = forth.popu();
    const n1 = forth.popu();
    forth.pushud(std.math.mulWide(UCell, n1, n2));
    forth.next();
}

pub fn @"FM/MOD"(forth: *Forth) noreturn {
    const n1 = forth.pop();
    const d1 = forth.popd();
    const q = @as(Cell, @intCast(@divFloor(d1, n1)));
    const r = @as(Cell, @intCast(@mod(d1, n1)));
    forth.push(r);
    forth.push(q);
    forth.next();
}

pub fn @"SM/REM"(forth: *Forth) noreturn {
    const n1 = forth.pop();
    const d1 = forth.popd();
    const q = @as(Cell, @intCast(@divTrunc(d1, n1)));
    const r = @as(Cell, @intCast(@rem(d1, n1)));
    forth.push(r);
    forth.push(q);
    forth.next();
}

pub fn @"UM/MOD"(forth: *Forth) noreturn {
    const n2 = forth.popu();
    const n1 = forth.popud();
    const q = @as(UCell, @intCast(n1 / n2));
    const r = @as(UCell, @intCast(n1 % n2));
    forth.pushu(r);
    forth.pushu(q);
    forth.next();
}

pub fn @"*/"(forth: *Forth) noreturn {
    const n3 = forth.pop();
    const n2 = forth.pop();
    const n1 = forth.pop();
    const prod: DCell = std.math.mulWide(Cell, n1, n2);
    forth.push(@intCast(@divTrunc(prod, n3)));
    forth.next();
}

pub fn @"*/MOD"(forth: *Forth) noreturn {
    const n3 = forth.pop();
    const n2 = forth.pop();
    const n1 = forth.pop();
    const prod: DCell = std.math.mulWide(Cell, n1, n2);
    const q: Cell = @intCast(@divTrunc(prod, n3));
    const r: Cell = @intCast(@rem(prod, n3));
    forth.push(r);
    forth.push(q);
    forth.next();
}

pub fn RSHIFT(forth: *Forth) noreturn {
    const u = forth.popu();
    const x1 = forth.popu();
    forth.pushu(x1 >> @intCast(u));
    forth.next();
}

pub fn LSHIFT(forth: *Forth) noreturn {
    const u = forth.popu();
    const x1 = forth.popu();
    forth.pushu(x1 << @intCast(u));
    forth.next();
}

pub fn @"2/"(forth: *Forth) noreturn {
    const x1 = forth.pop();
    forth.push(x1 >> 1);
    forth.next();
}

pub fn @"U<"(forth: *Forth) noreturn {
    const n2 = forth.popu();
    const n1 = forth.popu();
    forth.push(f_bool(n1 < n2));
    forth.next();
}

pub fn MIN(forth: *Forth) noreturn {
    const n2 = forth.pop();
    const n1 = forth.pop();
    forth.push(@min(n1, n2));
    forth.next();
}

pub fn MAX(forth: *Forth) noreturn {
    const n2 = forth.pop();
    const n1 = forth.pop();
    forth.push(@max(n1, n2));
    forth.next();
}

/// ( inc i end -- flag i+inc end )
/// flag is true if do .. +loop should be exited
/// From http://git.savannah.gnu.org/cgit/gforth.git/tree/prim#n429
pub fn @"+LOOP-COND"(forth: *Forth) noreturn {
    const nlimit = forth.pop();
    const n1 = forth.pop();
    const n = forth.pop();
    const olddiff = n1 -% nlimit;
    const n2 = n1 +% n;
    forth.push(f_bool(((olddiff ^ (olddiff +% n)) & (olddiff ^ n)) < 0));
    forth.push(n2);
    forth.push(nlimit);
    forth.next();
}

pub fn @">NUMBER"(forth: *Forth) noreturn {
    var len = forth.popu();
    var addr = @as([*]u8, @ptrFromInt(forth.popu()));
    var ud = forth.popud();
    while (len > 0) {
        ud *= forth.base;
        ud += std.fmt.charToDigit(addr[0], @intCast(forth.base)) catch break;
        addr += 1;
        len -= 1;
    }
    forth.pushud(ud);
    forth.pushu(@intFromPtr(addr));
    forth.pushu(len);
    forth.next();
}
