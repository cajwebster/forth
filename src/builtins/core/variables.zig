const Forth = @import("../../Forth.zig");

pub fn STATE(forth: *Forth) callconv(.C) noreturn {
    forth.pushu(@intFromPtr(&forth.state));
    @call(.always_tail, Forth.next, .{forth});
}

pub fn BASE(forth: *Forth) callconv(.C) noreturn {
    forth.pushu(@intFromPtr(&forth.base));
    @call(.always_tail, Forth.next, .{forth});
}

pub fn HERE(forth: *Forth) callconv(.C) noreturn {
    forth.pushu(@intFromPtr(forth.here));
    @call(.always_tail, Forth.next, .{forth});
}

pub fn LATEST(forth: *Forth) callconv(.C) noreturn {
    forth.pushu(@intFromPtr(&forth.dict));
    @call(.always_tail, Forth.next, .{forth});
}

pub fn @">IN"(forth: *Forth) callconv(.C) noreturn {
    forth.pushu(@intFromPtr(&forth.in));
    @call(.always_tail, Forth.next, .{forth});
}

pub fn R0(forth: *Forth) callconv(.C) noreturn {
    forth.pushu(@intFromPtr(&forth.rstack[0]));
    @call(.always_tail, Forth.next, .{forth});
}

pub fn RSP(forth: *Forth) callconv(.C) noreturn {
    forth.pushu(@intFromPtr(&forth.rsp));
    @call(.always_tail, Forth.next, .{forth});
}

pub fn SP(forth: *Forth) callconv(.C) noreturn {
    forth.pushu(@intFromPtr(&forth.sp));
    @call(.always_tail, Forth.next, .{forth});
}

pub fn @"INPUT-SP"(forth: *Forth) callconv(.C) noreturn {
    forth.pushu(@intFromPtr(&forth.input_stack.len));
    @call(.always_tail, Forth.next, .{forth});
}

pub fn SOURCE(forth: *Forth) callconv(.C) noreturn {
    forth.pushu(@intFromPtr(forth.input_buffer.ptr));
    forth.pushu(forth.input_buffer.len);
    @call(.always_tail, Forth.next, .{forth});
}
