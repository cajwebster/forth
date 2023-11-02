const Forth = @import("../Forth.zig");

const f_bool = Forth.f_bool;

/// ( d1 -- d2 )
pub fn DNEGATE(forth: *Forth) callconv(.C) noreturn {
    forth.pushd(-forth.popd());
    @call(.always_tail, Forth.next, .{forth});
}

/// ( d1 d2 -- d3 )
pub fn @"D+"(forth: *Forth) callconv(.C) noreturn {
    const d2 = forth.popd();
    const d1 = forth.popd();
    forth.pushd(d1 + d2);
    @call(.always_tail, Forth.next, .{forth});
}

/// ( d1 d2 -- d3 )
pub fn @"D-"(forth: *Forth) callconv(.C) noreturn {
    const d2 = forth.popd();
    const d1 = forth.popd();
    forth.pushd(d1 - d2);
    @call(.always_tail, Forth.next, .{forth});
}

/// ( d -- flag )
pub fn @"D0<"(forth: *Forth) callconv(.C) noreturn {
    const d = forth.popd();
    forth.push(f_bool(d < 0));
    @call(.always_tail, Forth.next, .{forth});
}

/// ( d -- flag )
pub fn @"D0="(forth: *Forth) callconv(.C) noreturn {
    const d = forth.popd();
    forth.push(f_bool(d == 0));
    @call(.always_tail, Forth.next, .{forth});
}

/// ( xd1 -- xd2 )
pub fn @"D2*"(forth: *Forth) callconv(.C) noreturn {
    forth.pushd(forth.popd() << 1);
    @call(.always_tail, Forth.next, .{forth});
}

/// ( xd1 -- xd2 )
pub fn @"D2/"(forth: *Forth) callconv(.C) noreturn {
    forth.pushd(forth.popd() >> 1);
    @call(.always_tail, Forth.next, .{forth});
}

/// ( d1 d2 -- flag )
pub fn @"D<"(forth: *Forth) callconv(.C) noreturn {
    const d2 = forth.popd();
    const d1 = forth.popd();
    forth.push(f_bool(d1 < d2));
    @call(.always_tail, Forth.next, .{forth});
}

/// ( d1 d2 -- flag )
pub fn @"DU<"(forth: *Forth) callconv(.C) noreturn {
    const ud2 = forth.popud();
    const ud1 = forth.popud();
    forth.push(f_bool(ud1 < ud2));
    @call(.always_tail, Forth.next, .{forth});
}

/// ( d1 d2 -- flag )
pub fn @"D="(forth: *Forth) callconv(.C) noreturn {
    const d2 = forth.popd();
    const d1 = forth.popd();
    forth.push(f_bool(d1 == d2));
    @call(.always_tail, Forth.next, .{forth});
}

/// ( d1 d2 -- flag )
pub fn @"D>"(forth: *Forth) callconv(.C) noreturn {
    const d2 = forth.popd();
    const d1 = forth.popd();
    forth.push(f_bool(d1 > d2));
    @call(.always_tail, Forth.next, .{forth});
}

/// ( d1 d2 -- d3 )
pub fn DMAX(forth: *Forth) callconv(.C) noreturn {
    const d2 = forth.popd();
    const d1 = forth.popd();
    forth.pushd(@max(d1, d2));
    @call(.always_tail, Forth.next, .{forth});
}

/// ( d1 d2 -- d3 )
pub fn DMIN(forth: *Forth) callconv(.C) noreturn {
    const d2 = forth.popd();
    const d1 = forth.popd();
    forth.pushd(@min(d1, d2));
    @call(.always_tail, Forth.next, .{forth});
}

/// ( d1 n -- d2 )
pub fn @"M+"(forth: *Forth) callconv(.C) noreturn {
    const n = forth.pop();
    const d = forth.popd();
    forth.pushd(d +% n);
    @call(.always_tail, Forth.next, .{forth});
}

/// ( d1 n1 n2 -- d2 )
pub fn @"M*/"(forth: *Forth) callconv(.C) noreturn {
    const TCell = @import("std").meta.Int(.signed, Forth.cell_bits * 3);
    const n2 = forth.pop();
    const n1 = forth.pop();
    const d = @as(TCell, forth.popd());

    forth.pushd(@intCast(@divTrunc(d * n1, n2)));

    @call(.always_tail, Forth.next, .{forth});
}
