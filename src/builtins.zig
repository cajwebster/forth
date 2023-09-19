const std = @import("std");

const Forth = @import("Forth.zig");

const Byte = Forth.Byte;
const Cell = Forth.Cell;
const DCell = Forth.DCell;
const UCell = Forth.UCell;
const UDCell = Forth.UDCell;
const Char = Forth.Char;
const Codeword = Forth.Codeword;
const DictEntry = Forth.DictEntry;
const cell_bits = Forth.cell_bits;
const cell_size = Forth.cell_size;

const f_bool = Forth.f_bool;

pub usingnamespace @import("builtins/arithmetic.zig");
pub usingnamespace @import("builtins/memory.zig");
pub usingnamespace @import("builtins/stack.zig");
pub usingnamespace @import("builtins/variables.zig");

pub fn LIT(forth: *Forth) noreturn {
    const val: UCell = @intFromPtr(forth.ip[0]);
    forth.pushu(val);
    forth.ip += 1;
    forth.next();
}

pub fn LITSTRING(forth: *Forth) noreturn {
    const len = @as(UCell, @intFromPtr(forth.ip[0]));
    forth.ip += 1;
    const addr = @as([*]const u8, @ptrCast(forth.ip));
    forth.ip += (len + Forth.cell_size - 1) / Forth.cell_size;

    forth.pushu(@intFromPtr(addr));
    forth.pushu(len);
    forth.next();
}

pub fn TYPE(forth: *Forth) noreturn {
    const len = forth.popu();
    const addr = @as([*]const u8, @ptrFromInt(forth.popu()));
    std.fmt.format(forth.stdout.writer(), "{s}", .{addr[0..len]}) catch forth.die("Error writing to stdout");
    forth.next();
}

pub fn @"DOCOL:"(forth: *Forth) noreturn {
    const name = forth.word(' ', .skip_leading);
    forth.add_word(name, Forth.docol, .{});
    forth.next();
}

pub fn @"NONAME-DOCOL:"(forth: *Forth) noreturn {
    forth.add_word("", Forth.docol, .{});
    forth.next();
}

pub fn CREATE(forth: *Forth) noreturn {
    const name = forth.word(' ', .skip_leading);
    forth.add_word(name, Forth.docreate, .{});
    forth.next();
}

pub fn @","(forth: *Forth) noreturn {
    forth.compile_cell(forth.pop());
    forth.next();
}

pub fn @"C,"(forth: *Forth) noreturn {
    forth.compile(@truncate(forth.popu()));
    forth.next();
}

pub fn IMMEDIATE(forth: *Forth) noreturn {
    forth.dict.?.flags.immediate = true;
    forth.next();
}

pub fn @"IMMEDIATE?"(forth: *Forth) noreturn {
    const dict_entry = @as(*DictEntry, @ptrFromInt(forth.popu()));
    forth.push(f_bool(dict_entry.flags.immediate));
    forth.next();
}

pub fn HIDDEN(forth: *Forth) noreturn {
    forth.dict.?.flags.hidden = !forth.dict.?.flags.hidden;
    forth.next();
}

/// : IN ( -- flag ) <builtin> ;
/// returns true if the parse area is non-empty
pub fn @"IN?"(forth: *Forth) noreturn {
    forth.push(f_bool(forth.in < forth.input_buffer.len));
    forth.next();
}

pub fn INTERPRET(forth: *Forth) noreturn {
    const word = forth.word(' ', .skip_leading);
    if (word.len > 0) {
        if (forth.find_word(word)) |dict_entry| {
            if (forth.state == 0 or dict_entry.flags.immediate) {
                forth.next_word = @ptrCast(&dict_entry.codeword);
                @call(.always_tail, forth.next_word[0], .{forth});
            } else {
                forth.compile_cell(@bitCast(@intFromPtr(&dict_entry.codeword)));
            }
        } else if (forth.parseInt(word) catch null) |num| {
            if (forth.state == 0) {
                forth.push(num);
            } else {
                forth.compile_cell(@bitCast(@intFromPtr(forth.lit)));
                forth.compile_cell(num);
            }
        } else {
            std.fmt.format(forth.stdout.writer(), " '{s}'?\n", .{word}) catch forth.die("Error writing to stdout");
            if (forth.state != 0) {
                // remove the word currently being compiled from the dictionary
                // and return to interpretation state
                forth.dict = forth.dict.?.prev;
                forth.state = 0;
            }
            forth.ip = &forth.quit;
            forth.input_stack = .{};
            forth.input_source = .{ .file = .{ .line = undefined, .len = 0, .file = forth.stdin } };
            forth.in = 0;
            forth.next();
        }
    }
    forth.next();
}

pub fn BYE(forth: *Forth) noreturn {
    forth.bye();
}

pub fn BRANCH(forth: *Forth) noreturn {
    const offset: UCell = @divExact(@intFromPtr(forth.ip[0]), cell_size);
    forth.ip += offset;
    forth.next();
}

pub fn @"0BRANCH"(forth: *Forth) noreturn {
    const offset: UCell = @divExact(@intFromPtr(forth.ip[0]), cell_size);
    if (forth.pop() == 0)
        forth.ip += offset
    else
        forth.ip += 1;
    forth.next();
}

pub fn REFILL(forth: *Forth) noreturn {
    const b = forth.refill();
    forth.push(f_bool(b));
    forth.next();
}

pub fn EMIT(forth: *Forth) noreturn {
    const c: Char = @truncate(forth.popu());
    std.fmt.format(forth.stdout.writer(), "{c}", .{c}) catch forth.die("Error writing to stdout");
    forth.next();
}

pub fn EXIT(forth: *Forth) noreturn {
    forth.ip = @ptrFromInt(forth.rpop());
    forth.next();
}

pub fn @"STR-INPUT"(forth: *Forth) noreturn {
    const len = forth.popu();
    const addr = @as([*]u8, @ptrFromInt(forth.popu()));
    const s = addr[0..len];
    forth.push_input_source();
    forth.input_source = .{ .str = s };
    forth.input_buffer = s;
    forth.in = 0;
    forth.next();
}

pub fn @"FILE-INPUT"(forth: *Forth) noreturn {
    const len = forth.popu();
    const addr = @as([*]u8, @ptrFromInt(forth.popu()));
    const path = addr[0..len];
    forth.push_input_source();
    const file = std.fs.cwd().openFile(path, .{}) catch forth.die("Error opening file");
    forth.input_source = .{ .file = .{ .line = undefined, .len = 0, .file = file } };
    forth.input_buffer = &.{};
    forth.in = 1;
    forth.next();
}

pub fn @"POP-INPUT"(forth: *Forth) noreturn {
    forth.pop_input_source();
    forth.next();
}

pub fn PARSE(forth: *Forth) noreturn {
    const c = @as(Char, @truncate(forth.popu()));
    const word = forth.word(c, .no_skip);
    forth.pushu(@intFromPtr(word.ptr));
    forth.pushu(word.len);
    forth.next();
}

pub fn @"PARSE-NAME"(forth: *Forth) noreturn {
    const word = forth.word(' ', .skip_leading);
    forth.pushu(@intFromPtr(word.ptr));
    forth.pushu(word.len);
    forth.next();
}

pub fn @"PARSE-S\\\""(forth: *Forth) noreturn {
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
    forth.next();
}

pub fn WORD(forth: *Forth) noreturn {
    const S = struct {
        var buffer: [257]u8 = undefined;
    };
    const buffer: *[257]u8 = &S.buffer;

    const c = @as(Char, @truncate(forth.popu()));
    const word = forth.word(c, .skip_leading);

    buffer[0] = @intCast(word.len);
    @memcpy(buffer[1 .. word.len + 1], word);
    forth.pushu(@intFromPtr(buffer));
    forth.next();
}

pub fn FIND(forth: *Forth) noreturn {
    const addr = @as([*]const u8, @ptrFromInt(forth.popu()));
    const len = addr[0];
    const s = addr[1 .. len + 1];
    if (forth.find_word(s)) |dict_entry| {
        forth.pushu(@intFromPtr(&dict_entry.codeword));
        forth.push(if (dict_entry.flags.immediate) 1 else -1);
    } else {
        forth.pushu(@intFromPtr(addr));
        forth.pushu(0);
    }
    forth.next();
}

pub fn EXECUTE(forth: *Forth) noreturn {
    forth.next_word = @ptrFromInt(forth.popu());
    @call(.always_tail, forth.next_word[0], .{forth});
}

var numeric_output_string: [2 * cell_bits + 2]Char = undefined;
var num_idx: u8 = numeric_output_string.len;

pub fn @"<#"(forth: *Forth) noreturn {
    num_idx = numeric_output_string.len;
    forth.next();
}

pub fn HOLD(forth: *Forth) noreturn {
    const c = @as(Char, @truncate(forth.popu()));
    num_idx -= 1;
    numeric_output_string[num_idx] = c;
    forth.next();
}

pub fn HOLDS(forth: *Forth) noreturn {
    const len = forth.popu();
    const addr = @as([*]const u8, @ptrFromInt(forth.popu()));
    num_idx -= @intCast(len);
    @memcpy(numeric_output_string[num_idx .. num_idx + len], addr[0..len]);
    forth.next();
}

pub fn @"#>"(forth: *Forth) noreturn {
    _ = forth.popd();
    const s = numeric_output_string[num_idx..];
    forth.pushu(@intFromPtr(s.ptr));
    forth.pushu(s.len);
    forth.next();
}

pub fn @">CFA"(forth: *Forth) noreturn {
    const addr = @as(*DictEntry, @ptrFromInt(forth.popu()));
    forth.pushu(@intFromPtr(&addr.codeword));
    forth.next();
}

pub fn @"CFA>"(forth: *Forth) noreturn {
    const addr = @as(*Codeword, @ptrFromInt(forth.popu()));
    const dict_entry = @fieldParentPtr(DictEntry, "codeword", addr);
    forth.pushu(@intFromPtr(dict_entry));
    forth.next();
}

pub fn @">DFA"(forth: *Forth) noreturn {
    const addr = forth.popu();
    forth.pushu(addr + @sizeOf(DictEntry));
    forth.next();
}

pub fn DOCOL(forth: *Forth) noreturn {
    forth.pushu(@intFromPtr(&Forth.docol));
    forth.next();
}

pub fn KEY(forth: *Forth) noreturn {
    forth.push(forth.key());
    forth.next();
}

pub fn @"ENVIRONMENT?"(forth: *Forth) noreturn {
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
    forth.next();
}

const envrionment = std.ComptimeStringMap(union(enum) {
    cell: Cell,
    dcell: DCell,
    ucell: UCell,
    udcell: UDCell,
    flag: bool,
}, .{
    .{ "/COUNTED-STRING", .{ .cell = std.math.maxInt(Char) } },
    .{ "/HOLD", .{ .cell = @as(Cell, @intCast(numeric_output_string.len)) } },
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

pub fn RAND(forth: *Forth) noreturn {
    forth.push(forth.rng.random().int(Cell));
    forth.next();
}

pub fn SRAND(forth: *Forth) noreturn {
    const s = forth.popu();
    if (cell_size < @sizeOf(u64))
        forth.rng.seed(@intCast(s))
    else
        forth.rng.seed(@truncate(s));
    forth.next();
}

pub fn @"DOES,"(forth: *Forth) noreturn {
    const len = @as(UCell, @intFromPtr(forth.ip[0]));
    forth.ip += 1;
    const latest = forth.dict.?;

    latest.codeword = Forth.dodoes;
    latest.flags.code_offset = @intCast(@as(DCell, @intCast(@intFromPtr(forth.ip))) - @as(DCell, @intCast(@intFromPtr(&latest.codeword))));

    forth.ip += (len - 1) / Forth.cell_size;

    forth.next();
}

pub fn @">BODY"(forth: *Forth) noreturn {
    const addr = @as(*const Codeword, @ptrFromInt(forth.popu()));
    const dict_entry = @fieldParentPtr(DictEntry, "codeword", addr);
    forth.pushu(@intFromPtr(dict_entry) + @sizeOf(DictEntry));
    forth.next();
}

pub fn @"SAVE-INPUT"(forth: *Forth) noreturn {
    forth.push_input_source();
    forth.pushu(forth.input_stack.len - 1);
    forth.pushu(1);
    forth.next();
}

pub fn @"RESTORE-INPUT"(forth: *Forth) noreturn {
    _ = forth.popu();
    const idx = forth.popu();
    const input_source = forth.input_stack.orderedRemove(idx);
    forth.restore_input_source(input_source);
    forth.push(f_bool(false));
    forth.next();
}

pub fn @"SOURCE-ID"(forth: *Forth) noreturn {
    forth.push(switch (forth.input_source) {
        .file => |file| if (file.file.handle == forth.stdin.handle) 0 else @panic("TODO: SOURCE-ID with file input"),
        .str => -1,
    });
    forth.next();
}
