const Forth = @import("../../Forth.zig");

pub fn STATE(forth: *Forth) noreturn {
    forth.pushu(@intFromPtr(&forth.state));
    forth.next();
}

pub fn BASE(forth: *Forth) noreturn {
    forth.pushu(@intFromPtr(&forth.base));
    forth.next();
}

pub fn HERE(forth: *Forth) noreturn {
    forth.pushu(@intFromPtr(forth.here));
    forth.next();
}

pub fn LATEST(forth: *Forth) noreturn {
    forth.pushu(@intFromPtr(&forth.dict));
    forth.next();
}

pub fn @">IN"(forth: *Forth) noreturn {
    forth.pushu(@intFromPtr(&forth.in));
    forth.next();
}

pub fn R0(forth: *Forth) noreturn {
    forth.pushu(@intFromPtr(&forth.rstack[0]));
    forth.next();
}

pub fn RSP(forth: *Forth) noreturn {
    forth.pushu(@intFromPtr(&forth.rsp));
    forth.next();
}

pub fn SOURCE(forth: *Forth) noreturn {
    forth.pushu(@intFromPtr(forth.input_buffer.ptr));
    forth.pushu(forth.input_buffer.len);
    forth.next();
}
