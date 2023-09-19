const builtin = @import("builtin");

pub usingnamespace if (builtin.os.tag != .freestanding)
    @import("io_impl/std.zig");
