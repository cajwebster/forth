const builtin = @import("builtin");

pub usingnamespace if (builtin.os.tag != .freestanding)
    @import("io_impl/std.zig");

pub const FileMode = enum {
    rw,
    ro,
    wo,
};
