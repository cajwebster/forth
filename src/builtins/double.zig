const Forth = @import("../Forth.zig");

const f_bool = Forth.f_bool;

/// ( d1 -- d2 )
pub fn DNEGATE(forth: *Forth) noreturn {
    forth.pushd(-forth.popd());
    forth.next();
}

/// ( d1 d2 -- d3 )
pub fn @"D+"(forth: *Forth) noreturn {
    const d2 = forth.popd();
    const d1 = forth.popd();
    forth.pushd(d1 + d2);
    forth.next();
}

/// ( d1 d2 -- d3 )
pub fn @"D-"(forth: *Forth) noreturn {
    const d2 = forth.popd();
    const d1 = forth.popd();
    forth.pushd(d1 - d2);
    forth.next();
}

/// ( d -- flag )
pub fn @"D0<"(forth: *Forth) noreturn {
    const d = forth.popd();
    forth.push(f_bool(d < 0));
    forth.next();
}

/// ( d -- flag )
pub fn @"D0="(forth: *Forth) noreturn {
    const d = forth.popd();
    forth.push(f_bool(d == 0));
    forth.next();
}

/// ( xd1 -- xd2 )
pub fn @"D2*"(forth: *Forth) noreturn {
    forth.pushd(forth.popd() << 1);
    forth.next();
}

/// ( xd1 -- xd2 )
pub fn @"D2/"(forth: *Forth) noreturn {
    forth.pushd(forth.popd() >> 1);
    forth.next();
}

/// ( d1 d2 -- flag )
pub fn @"D<"(forth: *Forth) noreturn {
    const d2 = forth.popd();
    const d1 = forth.popd();
    forth.push(f_bool(d1 < d2));
    forth.next();
}

/// ( d1 d2 -- flag )
pub fn @"DU<"(forth: *Forth) noreturn {
    const ud2 = forth.popud();
    const ud1 = forth.popud();
    forth.push(f_bool(ud1 < ud2));
    forth.next();
}

/// ( d1 d2 -- flag )
pub fn @"D="(forth: *Forth) noreturn {
    const d2 = forth.popd();
    const d1 = forth.popd();
    forth.push(f_bool(d1 == d2));
    forth.next();
}

/// ( d1 d2 -- flag )
pub fn @"D>"(forth: *Forth) noreturn {
    const d2 = forth.popd();
    const d1 = forth.popd();
    forth.push(f_bool(d1 > d2));
    forth.next();
}

/// ( d1 d2 -- d3 )
pub fn DMAX(forth: *Forth) noreturn {
    const d2 = forth.popd();
    const d1 = forth.popd();
    forth.pushd(@max(d1, d2));
    forth.next();
}

/// ( d1 d2 -- d3 )
pub fn DMIN(forth: *Forth) noreturn {
    const d2 = forth.popd();
    const d1 = forth.popd();
    forth.pushd(@min(d1, d2));
    forth.next();
}

/// ( d1 n -- d2 )
pub fn @"M+"(forth: *Forth) noreturn {
    const n = forth.pop();
    const d = forth.popd();
    forth.pushd(d +% n);
    forth.next();
}

/// ( d1 n1 n2 -- d2 )
pub fn @"M*/"(forth: *Forth) noreturn {
    const TCell = @import("std").meta.Int(.signed, Forth.cell_bits * 3);
    const n2 = forth.pop();
    const n1 = forth.pop();
    const d = @as(TCell, forth.popd());

    forth.pushd(@intCast(@divTrunc(d * n1, n2)));

    forth.next();
}
