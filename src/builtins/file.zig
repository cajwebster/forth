const std = @import("std");

const Forth = @import("../Forth.zig");
const io = Forth.io;
const f_bool = Forth.f_bool;

const FileMode = io.FileMode;

const ior_success = 0;
const ior_fail = -1;

/// ( c-addr u -- )
/// Opens the file at the path given by c-addr[0..u] and makes it the current
/// input source.
pub fn @"FILE-INPUT"(forth: *Forth) noreturn {
    const len = forth.popu();
    const addr = @as([*]u8, @ptrFromInt(forth.popu()));
    const path = addr[0..len];
    forth.push_input_source();
    const file = Forth.io.openFile(path, .ro) catch forth.die("Error opening file");
    forth.input_source = .{ .file = .{
        .line = undefined,
        .line_len = 0,
        .line_start = 0,
        .line_end = 0,
        .handle = file,
    } };
    forth.input_buffer = &.{};
    forth.in = 1;
    forth.next();
}

/// ( -- fam )
/// pushes the file access method for opening files in read/write mode
pub fn @"R/W"(forth: *Forth) noreturn {
    forth.push(@intFromEnum(FileMode.rw));
    forth.next();
}

/// ( -- fam )
/// pushes the file access method for opening files in write only mode
pub fn @"W/O"(forth: *Forth) noreturn {
    forth.push(@intFromEnum(FileMode.wo));
    forth.next();
}

/// ( -- fam )
/// pushes the file access method for opening files in read only mode
pub fn @"R/O"(forth: *Forth) noreturn {
    forth.push(@intFromEnum(FileMode.ro));
    forth.next();
}

/// ( c-addr u fam -- fileid ior )
/// Create a new file, truncating it if it already exists
pub fn @"CREATE-FILE"(forth: *Forth) noreturn {
    const fam = @as(FileMode, @enumFromInt(forth.pop()));
    const len = forth.popu();
    const addr = @as([*]const u8, @ptrFromInt(forth.popu()));
    if (io.createFile(addr[0..len], fam)) |id| {
        forth.push(id);
        forth.push(ior_success);
    } else |_| {
        forth.push(-1);
        forth.push(ior_fail);
    }
    forth.next();
}

/// ( c-addr u fam -- fileid ior )
/// Open an existing file
pub fn @"OPEN-FILE"(forth: *Forth) noreturn {
    const fam = @as(FileMode, @enumFromInt(forth.pop()));
    const len = forth.popu();
    const addr = @as([*]const u8, @ptrFromInt(forth.popu()));
    if (io.openFile(addr[0..len], fam)) |id| {
        forth.push(id);
        forth.push(ior_success);
    } else |_| {
        forth.push(-1);
        forth.push(ior_fail);
    }
    forth.next();
}

/// ( fileid -- ior )
/// closes an open file
pub fn @"CLOSE-FILE"(forth: *Forth) noreturn {
    if (io.closeFile(forth.pop())) |_|
        forth.push(ior_success)
    else |_|
        forth.push(ior_fail);
    forth.next();
}

/// ( c-addr u fileid -- ior )
/// Writes a string to a file followed by a newline
pub fn @"WRITE-LINE"(forth: *Forth) noreturn {
    const fileid = forth.pop();
    const len = forth.popu();
    const addr = @as([*]const u8, @ptrFromInt(forth.popu()));
    if (io.format(fileid, "{s}\n", .{addr[0..len]})) |_|
        forth.push(ior_success)
    else |_|
        forth.push(ior_fail);
    forth.next();
}

/// ( fileid -- ud ior )
pub fn @"FILE-POSITION"(forth: *Forth) noreturn {
    const fileid = forth.pop();
    if (io.filePosition(fileid)) |pos| {
        forth.pushud(pos);
        forth.push(ior_success);
    } else |_| {
        forth.pushud(0);
        forth.push(ior_fail);
    }
    forth.next();
}

/// ( c-addr u_1 fileid -- u_2 flag ior )
pub fn @"READ-LINE"(forth: *Forth) noreturn {
    const fileid = forth.pop();
    const len = forth.popu();
    const addr = @as([*]u8, @ptrFromInt(forth.popu()));
    const buf = addr[0..len];
    if (io.readLine(fileid, buf)) |line| {
        forth.pushu(line.len);
        forth.push(f_bool(true));
        forth.push(ior_success);
    } else |err| {
        switch (err) {
            error.EndOfStream => {
                forth.pushu(0);
                forth.push(f_bool(false));
                forth.push(ior_success);
            },
            else => {
                forth.pushu(0);
                forth.push(f_bool(false));
                forth.push(ior_success);
            },
        }
    }
    forth.next();
}

/// ( fileid -- ud ior )
pub fn @"FILE-SIZE"(forth: *Forth) noreturn {
    const fileid = forth.pop();
    if (io.fileSize(fileid)) |size| {
        forth.pushud(size);
        forth.push(ior_success);
    } else |_| {
        forth.pushud(0);
        forth.push(ior_fail);
    }
    forth.next();
}

/// ( ud fileid -- ior )
pub fn @"REPOSITION-FILE"(forth: *Forth) noreturn {
    const fileid = forth.pop();
    const pos = forth.popud();
    if (io.fileSeek(fileid, pos)) |_| {
        forth.push(ior_success);
    } else |_| {
        forth.push(ior_fail);
    }
    forth.next();
}

/// ( c-addr u fileid -- ior )
pub fn @"WRITE-FILE"(forth: *Forth) noreturn {
    const fileid = forth.pop();
    const len = forth.popu();
    const addr = @as([*]const u8, @ptrFromInt(forth.popu()));
    if (io.format(fileid, "{s}", .{addr[0..len]})) |_|
        forth.push(ior_success)
    else |_|
        forth.push(ior_fail);
    forth.next();
}

/// ( fileid -- ior )
pub fn @"FLUSH-FILE"(forth: *Forth) noreturn {
    const fileid = forth.pop();
    if (io.flushFile(fileid)) |_| {
        forth.push(ior_success);
    } else |_| {
        forth.push(ior_fail);
    }
    forth.next();
}

/// ( c-addr u_1 fileid -- u_2 ior )
pub fn @"READ-FILE"(forth: *Forth) noreturn {
    const fileid = forth.pop();
    const len = forth.popu();
    const addr = @as([*]u8, @ptrFromInt(forth.popu()));
    if (io.readFile(fileid, addr[0..len])) |read_len| {
        forth.pushu(read_len);
        forth.push(ior_success);
    } else |_| {
        forth.pushu(0);
        forth.push(ior_fail);
    }
    forth.next();
}

/// ( ud fileid -- ior )
pub fn @"RESIZE-FILE"(forth: *Forth) noreturn {
    const fileid = forth.pop();
    const length = forth.popud();
    if (io.resizeFile(fileid, length)) |_| {
        forth.push(ior_success);
    } else |_| {
        forth.push(ior_fail);
    }
    forth.next();
}

/// ( c-addr u -- ior )
pub fn @"DELETE-FILE"(forth: *Forth) noreturn {
    const len = forth.popu();
    const addr = @as([*]const u8, @ptrFromInt(forth.popu()));
    if (io.deleteFile(addr[0..len])) |_| {
        forth.push(ior_success);
    } else |_| {
        forth.push(ior_fail);
    }
    forth.next();
}

/// ( c-addr_1 u_1 c-addr_2 u_2 -- ior )
pub fn @"RENAME-FILE"(forth: *Forth) noreturn {
    const len2 = forth.popu();
    const addr2 = @as([*]const u8, @ptrFromInt(forth.popu()));
    const len1 = forth.popu();
    const addr1 = @as([*]const u8, @ptrFromInt(forth.popu()));
    if (io.renameFile(addr1[0..len1], addr2[0..len2])) |_| {
        forth.push(ior_success);
    } else |e| {
        std.debug.print("Error renaming file: {}\n", .{e});
        forth.push(ior_fail);
    }
    forth.next();
}

/// ( c-addr u -- x ior )
pub fn @"FILE-STATUS"(forth: *Forth) noreturn {
    const len = forth.popu();
    const addr = @as([*]const u8, @ptrFromInt(forth.popu()));
    if (io.fileStatus(addr[0..len])) |_| {
        forth.push(f_bool(true));
        forth.push(ior_success);
    } else |_| {
        forth.push(f_bool(false));
        forth.push(ior_fail);
    }
    forth.next();
}
