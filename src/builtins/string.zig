const std = @import("std");

const Forth = @import("../Forth.zig");

const UCell = Forth.UCell;

const f_bool = Forth.f_bool;

/// ( c-addr u1 -- c-addr u2 )
pub fn @"-TRAILING"(forth: *Forth) noreturn {
    const len = forth.popu();
    const addr = @as([*]const u8, @ptrFromInt(forth.popu()));
    const s = std.mem.trimRight(u8, addr[0..len], " ");
    forth.pushu(@intFromPtr(s.ptr));
    forth.pushu(s.len);
    forth.next();
}

/// ( c-addr1 u1 c-addr2 u2 -- c-addr3 u3 flag )
pub fn SEARCH(forth: *Forth) noreturn {
    const len2 = forth.popu();
    const addr2 = @as([*]const u8, @ptrFromInt(forth.popu()));
    const s2 = addr2[0..len2];
    const len1 = forth.popu();
    const addr1 = @as([*]const u8, @ptrFromInt(forth.popu()));
    const s1 = addr1[0..len1];
    if (std.mem.indexOf(u8, s1, s2)) |idx| {
        const s3 = s1[idx..];
        forth.pushu(@intFromPtr(s3.ptr));
        forth.pushu(s3.len);
        forth.push(f_bool(true));
    } else {
        forth.pushu(@intFromPtr(s1.ptr));
        forth.pushu(s1.len);
        forth.push(f_bool(false));
    }
    forth.next();
}

/// ( c-addr1 u1 c-addr2 u2 -- n )
pub fn COMPARE(forth: *Forth) noreturn {
    const len2 = forth.popu();
    const addr2 = @as([*]const u8, @ptrFromInt(forth.popu()));
    const s2 = addr2[0..len2];
    const len1 = forth.popu();
    const addr1 = @as([*]const u8, @ptrFromInt(forth.popu()));
    const s1 = addr1[0..len1];
    forth.push(switch (std.mem.order(u8, s1, s2)) {
        .lt => -1,
        .gt => 1,
        .eq => 0,
    });
    forth.next();
}

/// ( c-addr1 c-addr2 u -- )
pub fn CMOVE(forth: *Forth) noreturn {
    const len = forth.popu();
    const addr2 = @as([*]u8, @ptrFromInt(forth.popu()));
    const addr1 = @as([*]const u8, @ptrFromInt(forth.popu()));
    std.mem.copyForwards(u8, addr2[0..len], addr1[0..len]);
    forth.next();
}

/// ( c-addr1 c-addr2 u -- )
pub fn @"CMOVE>"(forth: *Forth) noreturn {
    const len = forth.popu();
    const addr2 = @as([*]u8, @ptrFromInt(forth.popu()));
    const addr1 = @as([*]const u8, @ptrFromInt(forth.popu()));
    std.mem.copyBackwards(u8, addr2[0..len], addr1[0..len]);
    forth.next();
}

/// ( c-addr1 u1 c-addr2 -- c-addr2 u2)
pub fn UNESCAPE(forth: *Forth) noreturn {
    const addr2 = @as([*]u8, @ptrFromInt(forth.popu()));
    const len = forth.popu();
    const addr1 = @as([*]u8, @ptrFromInt(forth.popu()));
    var i: UCell = 0;
    for (addr1[0..len]) |c| {
        addr2[i] = c;
        if (c == '%') {
            addr2[i + 1] = c;
            i += 2;
        } else {
            i += 1;
        }
    }
    forth.pushu(@intFromPtr(addr2));
    forth.pushu(i);
    forth.next();
}
