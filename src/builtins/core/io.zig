const Forth = @import("../../Forth.zig");

const Cell = Forth.Cell;
const DCell = Forth.Cell;
const Char = Forth.Char;

const cell_bits = Forth.cell_bits;

const f_bool = Forth.f_bool;

/// ( -- flag )
/// flag is true if the parse area is non-empty
pub fn @"IN?"(forth: *Forth) callconv(.C) noreturn {
    forth.push(f_bool(forth.in < forth.input_buffer.len));
    @call(.always_tail, Forth.next, .{forth});
}

/// ( c-addr u -- )
/// Makes the given string the input source
pub fn @"STR-INPUT"(forth: *Forth) callconv(.C) noreturn {
    const len = forth.popu();
    const addr = @as([*]u8, @ptrFromInt(forth.popu()));
    const s = addr[0..len];
    forth.push_input_source();
    forth.input_source = .{ .str = s };
    forth.input_buffer = s;
    forth.in = 0;
    @call(.always_tail, Forth.next, .{forth});
}

/// ( -- )
pub fn @"PUSH-INPUT"(forth: *Forth) callconv(.C) noreturn {
    forth.push_input_source();
    @call(.always_tail, Forth.next, .{forth});
}

/// ( -- )
/// Returns to the previous input source
pub fn @"POP-INPUT"(forth: *Forth) callconv(.C) noreturn {
    forth.pop_input_source();
    @call(.always_tail, Forth.next, .{forth});
}

/// ( -- )
pub fn @"DROP-INPUT"(forth: *Forth) callconv(.C) noreturn {
    _ = forth.input_stack.pop();
    @call(.always_tail, Forth.next, .{forth});
}

/// ( -- )
/// Clears the input source stack and sets the input source to stdin
pub fn @"CLEAR-INPUT-STACK"(forth: *Forth) callconv(.C) noreturn {
    forth.input_stack = .{};
    forth.input_source = .{ .stdin = undefined };
    @call(.always_tail, Forth.next, .{forth});
}

/// ( -- x_n ... x_1 n )
pub fn @"SAVE-INPUT"(forth: *Forth) callconv(.C) noreturn {
    switch (forth.input_source) {
        .stdin => forth.push(0),
        .file => |file| {
            forth.pushud(file.line_end);
            forth.pushud(file.line_start);
            forth.pushu(forth.in);
            forth.push(file.handle);
            forth.push(6);
        },
        .str => |str| {
            forth.pushu(@intFromPtr(str.ptr));
            forth.pushu(str.len);
            forth.pushu(forth.in);
            forth.push(3);
        },
    }
    @call(.always_tail, Forth.next, .{forth});
}

/// ( x_n ... x_1 n -- flag )
pub fn @"RESTORE-INPUT"(forth: *Forth) callconv(.C) noreturn {
    const num_args = forth.popu();
    switch (num_args) {
        6 => {
            const handle = forth.pop();
            const in = forth.popu();
            const line_start = forth.popud();
            const line_end = forth.popud();
            if (forth.input_source == .file and
                forth.input_source.file.handle == handle)
            {
                Forth.io.fileSeek(handle, line_start) catch unreachable;
                forth.input_buffer = Forth.io.readLine(
                    handle,
                    &forth.input_source.file.line,
                ) catch unreachable;
                forth.input_source.file.line_start = line_start;
                forth.input_source.file.line_end = line_end;
                forth.in = in;
                forth.push(f_bool(false));
            } else {
                forth.push(f_bool(true));
            }
        },
        3 => {
            const in = forth.popu();
            const len = forth.popu();
            const addr = @as([*]const u8, @ptrFromInt(forth.popu()));
            if (forth.input_source == .str and
                forth.input_source.str.ptr == addr and
                forth.input_source.str.len == len)
            {
                forth.in = in;
                forth.push(f_bool(false));
            } else {
                forth.push(f_bool(true));
            }
        },
        else => {
            for (0..num_args) |_| _ = forth.pop();
            forth.push(f_bool(true));
        },
    }
    @call(.always_tail, Forth.next, .{forth});
}

/// ( -- 0 | -1 | fileid )
pub fn @"SOURCE-ID"(forth: *Forth) callconv(.C) noreturn {
    forth.push(switch (forth.input_source) {
        .stdin => 0,
        .file => |file| file.handle,
        .str => -1,
    });
    @call(.always_tail, Forth.next, .{forth});
}

/// ( -- flag )
/// Attempts to refill the input buffer. Flag is true if the operation was
/// successful
pub fn REFILL(forth: *Forth) callconv(.C) noreturn {
    const b = forth.refill();
    forth.push(f_bool(b));
    @call(.always_tail, Forth.next, .{forth});
}

/// ( -- c )
/// Receives one character of input from the keyboard
pub fn KEY(forth: *Forth) callconv(.C) noreturn {
    forth.push(forth.key());
    @call(.always_tail, Forth.next, .{forth});
}

/// ( c -- )
/// Displays a single character
pub fn EMIT(forth: *Forth) callconv(.C) noreturn {
    const c: Char = @truncate(forth.popu());
    Forth.io.format(null, "{c}", .{c}) catch forth.die("Error writing to stdout");
    @call(.always_tail, Forth.next, .{forth});
}

/// ( c-addr u -- )
/// If u is greater than zero, display the character string specified by c-addr
/// and u.
pub fn TYPE(forth: *Forth) callconv(.C) noreturn {
    const len = forth.popu();
    const addr = @as([*]const u8, @ptrFromInt(forth.popu()));
    Forth.io.format(null, "{s}", .{addr[0..len]}) catch {
        forth.die("Error writing to stdout");
    };
    @call(.always_tail, Forth.next, .{forth});
}

/// ( char "ccc<char>" -- c-addr u )
/// Parse ccc delimited by char without skipping leading delimiters. c-addr is
/// within the input buffer.
pub fn PARSE(forth: *Forth) callconv(.C) noreturn {
    const c = @as(Char, @truncate(forth.popu()));
    const word = forth.word(c, .no_skip);
    forth.pushu(@intFromPtr(word.ptr));
    forth.pushu(word.len);
    @call(.always_tail, Forth.next, .{forth});
}

/// ( "<spaces>name<space>" -- c-addr u )
/// Parse a name delimited by spaces, skipping leading delimiters. c-addr is
/// within the input buffer.
pub fn @"PARSE-NAME"(forth: *Forth) callconv(.C) noreturn {
    const word = forth.word(' ', .skip_leading);
    forth.pushu(@intFromPtr(word.ptr));
    forth.pushu(word.len);
    @call(.always_tail, Forth.next, .{forth});
}

/// ( c-addr u -- n 1 | d -1 | 0 )
pub fn @"PARSE-NUM"(forth: *Forth) callconv(.C) noreturn {
    const len = forth.popu();
    const addr = @as([*]const u8, @ptrFromInt(forth.popu()));
    const s = addr[0..len];
    if (s.len > 0 and s[s.len - 1] == '.') {
        if (forth.parseInt(DCell, s[0 .. s.len - 1])) |d| {
            forth.pushd(d);
            forth.push(-1);
        } else |_| {
            forth.push(0);
        }
    } else if (forth.parseInt(Cell, addr[0..len])) |n| {
        forth.push(n);
        forth.push(1);
    } else |_| {
        forth.push(0);
    }
    @call(.always_tail, Forth.next, .{forth});
}

/// ( "str"" -- c-addr u )
/// Parses a quoted string containing escape characters
pub fn @"PARSE-S\\\""(forth: *Forth) callconv(.C) noreturn {
    const s = blk: {
        if (forth.in >= forth.input_buffer.len) break :blk &.{};
        const s = forth.input_buffer[forth.in..];
        var i: usize = 0;
        while (i < s.len) : (i += 1) {
            if (s[i] == '\\') {
                i += 1;
                continue;
            }
            if (s[i] == '"') break;
        }
        forth.in += i + 1;
        break :blk s[0..i];
    };
    forth.pushu(@intFromPtr(s.ptr));
    forth.pushu(s.len);
    @call(.always_tail, Forth.next, .{forth});
}

/// ( "<spaces>name" -- c-addr )
/// Parses a name and returns the address of a counted string
pub fn WORD(forth: *Forth) callconv(.C) noreturn {
    const S = struct {
        var buffer: [257]u8 = undefined;
    };
    const buffer: *[257]u8 = &S.buffer;

    const c = @as(Char, @truncate(forth.popu()));
    const word = forth.word(c, .skip_leading);

    buffer[0] = @intCast(word.len);
    @memcpy(buffer[1 .. word.len + 1], word);
    forth.pushu(@intFromPtr(buffer));
    @call(.always_tail, Forth.next, .{forth});
}

pub var numeric_output_string: [2 * cell_bits + 2]Char = undefined;
var num_idx: u8 = numeric_output_string.len;

/// ( -- )
/// Initializes the numeric output string
pub fn @"<#"(forth: *Forth) callconv(.C) noreturn {
    num_idx = numeric_output_string.len;
    @call(.always_tail, Forth.next, .{forth});
}

/// ( c -- )
/// Adds a character to the numeric output string
pub fn HOLD(forth: *Forth) callconv(.C) noreturn {
    const c = @as(Char, @truncate(forth.popu()));
    num_idx -= 1;
    numeric_output_string[num_idx] = c;
    @call(.always_tail, Forth.next, .{forth});
}

/// ( c-addr u -- )
/// Adds a string to the numeric output string
pub fn HOLDS(forth: *Forth) callconv(.C) noreturn {
    const len = forth.popu();
    const addr = @as([*]const u8, @ptrFromInt(forth.popu()));
    num_idx -= @intCast(len);
    @memcpy(numeric_output_string[num_idx .. num_idx + len], addr[0..len]);
    @call(.always_tail, Forth.next, .{forth});
}

/// ( xd -- c-addr u )
/// Drops xd and returns the numeric output string
pub fn @"#>"(forth: *Forth) callconv(.C) noreturn {
    _ = forth.popd();
    const s = numeric_output_string[num_idx..];
    forth.pushu(@intFromPtr(s.ptr));
    forth.pushu(s.len);
    @call(.always_tail, Forth.next, .{forth});
}
