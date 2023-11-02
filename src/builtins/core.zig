const std = @import("std");

const Forth = @import("../Forth.zig");

const Cell = Forth.Cell;
const UCell = Forth.UCell;
const DCell = Forth.DCell;
const UDCell = Forth.UDCell;
const Char = Forth.Char;

const cell_size = Forth.cell_size;

const f_bool = Forth.f_bool;

pub usingnamespace @import("core/arithmetic.zig");
pub usingnamespace @import("core/dictionary.zig");
pub usingnamespace @import("core/io.zig");
pub usingnamespace @import("core/memory.zig");
pub usingnamespace @import("core/stack.zig");
pub usingnamespace @import("core/variables.zig");

/// ( -- n )
/// Reads a literal cell value compiled immediately following this word and
/// pushes it to the stack.
pub fn LIT(forth: *Forth) callconv(.C) noreturn {
    const val: UCell = @intFromPtr(forth.ip[0]);
    forth.pushu(val);
    forth.ip += 1;
    @call(.always_tail, Forth.next, .{forth});
}

/// ( -- c-addr u )
/// Reads the length of the string after this word. The string immediately
/// follows the length, and the next word is aligned to the nearest cell.
pub fn LITSTRING(forth: *Forth) callconv(.C) noreturn {
    const len = @as(UCell, @intFromPtr(forth.ip[0]));
    forth.ip += 1;
    const addr = @as([*]const u8, @ptrCast(forth.ip));
    forth.ip += (len + Forth.cell_size - 1) / Forth.cell_size;

    forth.pushu(@intFromPtr(addr));
    forth.pushu(len);
    @call(.always_tail, Forth.next, .{forth});
}

/// ( -- )
/// Reads on offset after this word, and adds that offset to forth.ip.
pub fn BRANCH(forth: *Forth) callconv(.C) noreturn {
    const offset: UCell = @divExact(@intFromPtr(forth.ip[0]), cell_size);
    forth.ip += offset;
    @call(.always_tail, Forth.next, .{forth});
}

/// ( flag -- )
/// Same as branch, but only if flag is false
pub fn @"0BRANCH"(forth: *Forth) callconv(.C) noreturn {
    const offset: UCell = @divExact(@intFromPtr(forth.ip[0]), cell_size);
    if (forth.pop() == 0)
        forth.ip += offset
    else
        forth.ip += 1;
    @call(.always_tail, Forth.next, .{forth});
}

pub fn @"DOES,"(forth: *Forth) callconv(.C) noreturn {
    const len = @as(UCell, @intFromPtr(forth.ip[0]));
    forth.ip += 1;
    const latest = forth.dict.?;

    latest.codeword = Forth.dodoes;
    latest.flags.code_offset = @intCast(@as(
        DCell,
        @intCast(@intFromPtr(forth.ip)),
    ) - @as(
        DCell,
        @intCast(@intFromPtr(&latest.codeword)),
    ));

    forth.ip += (len - 1) / Forth.cell_size;

    @call(.always_tail, Forth.next, .{forth});
}

/// ( i * x xt -- j * x )
/// Executes xt
pub fn EXECUTE(forth: *Forth) callconv(.C) noreturn {
    forth.next_word = @ptrFromInt(forth.popu());
    @call(.always_tail, forth.next_word[0], .{forth});
}

/// ( -- ) ( R: nest-sys -- )
/// Returns from the current word to the calling word
pub fn EXIT(forth: *Forth) callconv(.C) noreturn {
    forth.ip = @ptrFromInt(forth.rpop());
    @call(.always_tail, Forth.next, .{forth});
}

/// ( -- )
/// Returns control to the host operating system
pub fn BYE(forth: *Forth) callconv(.C) noreturn {
    forth.bye();
}

/// ( i * x "<spaces>name" -- j * x )
/// Interprets a single word, as described in section 3.4 of the Forth 2012
/// standard.
pub fn INTERPRET(forth: *Forth) callconv(.C) noreturn {
    const word = forth.word(' ', .skip_leading);
    if (word.len > 0) {
        if (forth.find_word(word)) |dict_entry| {
            if (forth.state == 0 or dict_entry.flags.immediate) {
                forth.next_word = @ptrCast(&dict_entry.codeword);
                @call(.always_tail, forth.next_word[0], .{forth});
            } else {
                forth.compile_cell(@bitCast(@intFromPtr(&dict_entry.codeword)));
            }
        } else if (forth.parseInt(Cell, word)) |num| {
            if (forth.state == 0) {
                forth.push(num);
            } else {
                forth.compile_cell(@bitCast(@intFromPtr(forth.lit)));
                forth.compile_cell(num);
            }
        } else |_| {
            Forth.io.format(null, " '{s}'?\n", .{word}) catch forth.die("Error writing to stdout");
            if (forth.state != 0) {
                // remove the word currently being compiled from the dictionary
                // and return to interpretation state
                forth.dict = forth.dict.?.prev;
                forth.state = 0;
            }
            forth.ip = &forth.quit;
            forth.input_stack = .{};
            forth.input_source = .{ .stdin = undefined };
            forth.in = 0;
            @call(.always_tail, Forth.next, .{forth});
        }
    }
    @call(.always_tail, Forth.next, .{forth});
}

pub fn @"ENVIRONMENT?"(forth: *Forth) callconv(.C) noreturn {
    const len = forth.popu();
    const addr = @as([*]u8, @ptrFromInt(forth.popu()));
    if (envrionment.get(addr[0..len])) |value| {
        switch (value) {
            .cell => |cell| forth.push(cell),
            .dcell => |dcell| forth.pushd(dcell),
            .ucell => |ucell| forth.pushu(ucell),
            .udcell => |udcell| forth.pushud(udcell),
            .flag => |flag| forth.push(f_bool(flag)),
        }
        forth.push(f_bool(true));
    } else {
        forth.push(f_bool(false));
    }
    @call(.always_tail, Forth.next, .{forth});
}

const envrionment = std.ComptimeStringMap(union(enum) {
    cell: Cell,
    dcell: DCell,
    ucell: UCell,
    udcell: UDCell,
    flag: bool,
}, .{
    .{ "/COUNTED-STRING", .{ .cell = std.math.maxInt(Char) } },
    .{ "/HOLD", .{
        .cell = @as(Cell, @intCast(@This().numeric_output_string.len)),
    } },
    .{ "ADDRESS-UNIT-BITS", .{ .cell = 8 } },
    .{ "FLOORED", .{ .flag = false } },
    .{ "MAX-CHAR", .{ .ucell = std.math.maxInt(Char) } },
    .{ "MAX-D", .{ .dcell = std.math.maxInt(DCell) } },
    .{ "MAX-N", .{ .cell = std.math.maxInt(Cell) } },
    .{ "MAX-U", .{ .ucell = std.math.maxInt(UCell) } },
    .{ "MAX-UD", .{ .udcell = std.math.maxInt(UDCell) } },
    .{ "RETURN-STACK-CELLS", .{ .cell = Forth.stack_size } },
    .{ "STACK-CELLS", .{ .cell = Forth.stack_size } },
});

pub fn RAND(forth: *Forth) callconv(.C) noreturn {
    forth.push(forth.rng.random().int(Cell));
    @call(.always_tail, Forth.next, .{forth});
}

pub fn SRAND(forth: *Forth) callconv(.C) noreturn {
    const s = forth.popu();
    if (cell_size < @sizeOf(u64))
        forth.rng.seed(@intCast(s))
    else
        forth.rng.seed(@truncate(s));
    @call(.always_tail, Forth.next, .{forth});
}
