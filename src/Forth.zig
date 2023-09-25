const std = @import("std");
const root = @import("root");

const cfg = if (@hasDecl(root, "forth_cfg")) root.forth_cfg else Cfg{};

const builtins = @import("builtins.zig");
pub const io = @import("io.zig");
pub const Cfg = @import("Cfg.zig");

pub const cell_size = @sizeOf(usize);
pub const cell_bits = cell_size * 8;

pub const Byte = std.meta.Int(.signed, 8);
pub const UByte = std.meta.Int(.unsigned, 8);
pub const Char = UByte;
pub const Cell = std.meta.Int(.signed, cell_bits);
pub const UCell = std.meta.Int(.unsigned, cell_bits);
pub const DCell = std.meta.Int(.signed, cell_bits * 2);
pub const UDCell = std.meta.Int(.unsigned, cell_bits * 2);
pub const Codeword = *const fn (*Forth) noreturn;
const Forth = @This();

pub const f_true: Cell = ~@as(Cell, 0);
pub const f_false: Cell = 0;
pub fn f_bool(b: bool) Cell {
    return if (b) f_true else f_false;
}

pub const DictEntry = extern struct {
    pub const Flags = packed struct(Cell) {
        immediate: bool = false,
        hidden: bool = false,
        code_offset: i16 = 0,
        _: std.meta.Int(.unsigned, cell_bits - 18) = 0,
    };
    prev: ?*DictEntry,
    name: [32]Char,
    flags: Flags,
    codeword: Codeword,
};

pub const max_line_len = cfg.max_line_len;
const InputSource = union(enum) {
    stdin: struct {
        line: [max_line_len]Char,
        line_len: UCell,
    },
    file: struct {
        line: [max_line_len]Char,
        line_len: UCell,
        line_start: UDCell,
        line_end: UDCell,
        handle: io.FileId,
    },
    str: []u8,
};
pub const InputStackItem = struct {
    source: InputSource,
    in: UCell,
};
const InputStack = std.BoundedArray(InputStackItem, 32);

pub const mem_size = cfg.mem_size;
pub const stack_size = cfg.stack_size;

state: Cell = 0,
base: UCell = 10,

mem: [mem_size]UByte align(@alignOf(Cell)) = undefined,
here: [*]UByte = undefined,
dict: ?*DictEntry = null,

stack: [stack_size]Cell = undefined,
sp: [*]Cell = undefined,
rstack: [stack_size]UCell = undefined,
rsp: [*]UCell = undefined,
leavestack: [stack_size]UCell = undefined,
lsp: [*]UCell = undefined,

ip: [*]const [*]const Codeword = undefined,
next_word: [*]const Codeword = undefined,

input_source: InputSource = .{ .stdin = undefined },
input_stack: InputStack = InputStack{},
input_buffer: []u8 = &.{},
in: UCell = 0,

lit: *const Codeword = undefined,
quit: [1][*]const Codeword = undefined,

rng: std.rand.DefaultPrng = std.rand.DefaultPrng.init(0),

pub fn init(self: *Forth) noreturn {
    self.here = &self.mem;
    self.sp = &self.stack;
    self.rsp = &self.rstack;
    self.lsp = &self.leavestack;

    self.rng.seed(@bitCast(std.time.timestamp()));

    inline for (@typeInfo(builtins).Struct.decls) |builtin| {
        self.add_word(builtin.name, @field(builtins, builtin.name), .{});
    }

    // : [ ( -- ) \ Enter interpretation state
    //     FALSE
    //     STATE
    //     !
    // ; IMMEDIATE
    self.compile_word("[", .{ .immediate = true }, &.{
        .{ .word = "LIT" },
        .{ .cell = f_false },
        .{ .word = "STATE" },
        .{ .word = "!" },
        .{ .word = "EXIT" },
    });

    // : ] ( -- ) \ Enter compilation state
    //     TRUE
    //     STATE
    //     !
    // ;
    self.compile_word("]", .{}, &.{
        .{ .word = "LIT" },
        .{ .cell = f_true },
        .{ .word = "STATE" },
        .{ .word = "!" },
        .{ .word = "EXIT" },
    });

    // : : ( "<spaces>name" -- ) \ Creates a new dictionary entry for name,
    //     DOCOL:                \ hides the new entry and starts compiling.
    //     HIDDEN
    //     ]
    // ;
    self.compile_word(":", .{}, &.{
        .{ .word = "DOCOL:" },
        .{ .word = "HIDDEN" },
        .{ .word = "]" },
        .{ .word = "EXIT" },
    });

    // : ; ( -- ) \ Finishes the compilation of the current word and unhides it.
    //     POSTPONE EXIT
    //     HIDDEN
    //     [
    // ; IMMEDIATE
    self.compile_word(";", .{ .immediate = true }, &.{
        .{ .word = "LIT" },
        .{ .word = "EXIT" },
        .{ .word = "," },
        .{ .word = "HIDDEN" },
        .{ .word = "[" },
        .{ .word = "EXIT" },
    });

    // : EVALUATE ( i * x c-addr u -- j * x ) \ Interprets a string.
    //     STR-INPUT
    //     BEGIN
    //         IN?
    //     WHILE
    //         INTERPRET
    //     REPEAT
    //     POP-INPUT
    // ;
    self.compile_word("EVALUATE", .{}, &.{
        .{ .word = "STR-INPUT" },

        .{ .word = "IN?" },
        .{ .word = "0BRANCH" }, // branch to outer BRANCH
        .{ .cell = 4 * cell_size },
        .{ .word = "INTERPRET" },
        .{ .word = "BRANCH" }, // branch to IN?
        .{ .cell = -5 * cell_size },

        .{ .word = "POP-INPUT" },
        .{ .word = "EXIT" },
    });

    // : QUIT ( -- ) ( R: i * x -- ) \ Empties the return stack and starts
    //     R0 RSP !                  \ interpreting from stdin
    //     CLEAR-INPUT-STACK
    //     BEGIN
    //         REFILL
    //     WHILE
    //         BEGIN
    //             IN?
    //         WHILE
    //             INTERPRET
    //         REPEAT
    //         STATE @ 0= IF
    //             ." OK" CR
    //         THEN
    //     REPEAT
    //     BYE
    // ;
    self.compile_word("QUIT", .{}, &.{
        .{ .word = "R0" },
        .{ .word = "RSP" },
        .{ .word = "!" },
        .{ .word = "CLEAR-INPUT-STACK" },

        .{ .word = "REFILL" },
        .{ .word = "0BRANCH" }, // branch to BYE
        // TODO: these offsets won't work with 16-bit cells, since the prompt
        // string would be more than one cell long
        .{ .cell = 18 * cell_size },

        .{ .word = "IN?" },
        .{ .word = "0BRANCH" }, // branch to outer BRANCH
        .{ .cell = 4 * cell_size },
        .{ .word = "INTERPRET" },
        .{ .word = "BRANCH" }, // branch to IN?
        .{ .cell = -5 * cell_size },

        .{ .word = "STATE" },
        .{ .word = "@" },
        .{ .word = "0=" },
        .{ .word = "0BRANCH" },
        .{ .cell = 5 * cell_size }, // branch past type
        .{ .word = "LITSTRING" },
        .{ .string = " OK\n" },
        .{ .word = "TYPE" },
        .{ .word = "BRANCH" }, // branch to REFILL
        .{ .cell = -19 * cell_size },

        .{ .word = "BYE" },
    });

    // : _START \ Interpreter entry point
    //     S" <contents of forth.f>" EVALUATE
    //     QUIT
    // ;
    self.compile_word("_START", .{}, &.{
        .{ .word = "LITSTRING" },
        .{ .string = @embedFile("forth.f") },
        .{ .word = "EVALUATE" },
        .{ .word = "QUIT" },
    });

    self.lit = &self.find_word("LIT").?.codeword;

    const quit = self.find_word("QUIT").?;
    self.quit = .{@ptrCast(&quit.codeword)};

    const start = [_][*]Codeword{
        @ptrCast(&self.find_word("_START").?.codeword),
    };
    self.ip = &start;
    io.format(null, "Starting...\n", .{}) catch self.die("Error writing to stdout");
    self.next();
}

pub fn parseInt(self: *Forth, buf: []const Char) std.fmt.ParseIntError!Cell {
    if (buf.len == 0) return error.InvalidCharacter;
    return switch (buf[0]) {
        '#' => std.fmt.parseInt(Cell, buf[1..], 10),
        '$' => std.fmt.parseInt(Cell, buf[1..], 16),
        '%' => std.fmt.parseInt(Cell, buf[1..], 2),
        '\'' => if (buf.len == 3 and buf[2] == '\'')
            buf[1]
        else
            error.InvalidCharacter,
        else => std.fmt.parseInt(Cell, buf, @intCast(self.base)),
    };
}

pub fn key(self: *Forth) Cell {
    return io.readChar() catch |e| switch (e) {
        error.EndOfStream => -1,
        else => self.die("Error reading key"),
    };
}

pub fn refill(self: *Forth) bool {
    switch (self.input_source) {
        .stdin => |*stdin| {
            self.input_buffer = &.{};
            self.input_buffer = io.readLine(
                null,
                &stdin.line,
            ) catch |e| switch (e) {
                error.EndOfStream => return false,
                else => self.die("Error reading from stdin"),
            };
            self.in = 0;
            stdin.line_len = self.input_buffer.len;
            return true;
        },
        .file => |*file| {
            self.input_buffer = &.{};
            file.line_start = io.filePosition(file.handle) catch
                self.die("Error getting file position");
            self.input_buffer = io.readLine(
                file.handle,
                &file.line,
            ) catch |e| switch (e) {
                error.EndOfStream => return false,
                else => self.die("Error reading from file"),
            };
            self.in = 0;
            file.line_len = self.input_buffer.len;
            file.line_end = io.filePosition(file.handle) catch
                self.die("Error getting file position");
            return true;
        },
        .str => return false,
    }
}

pub fn word(
    self: *Forth,
    delim: Char,
    trim_leading: enum { skip_leading, no_skip },
) []const Char {
    if (self.in >= self.input_buffer.len) return &.{};

    const delims: []const Char =
        if (delim == ' ')
        &std.ascii.whitespace
    else
        &.{delim};

    var input: []const Char = self.input_buffer[self.in..];
    if (trim_leading == .skip_leading) {
        self.in += input.len;
        input = std.mem.trimLeft(Char, input, delims);
        self.in -= input.len;
    }
    input = input[0 .. std.mem.indexOfAny(Char, input, delims) orelse
        input.len];
    self.in += input.len + 1;

    return input;
}

pub fn emit(self: *const Forth, c: Char) void {
    std.fmt.format(self.stdout.writer(), "{c}", .{c}) catch self.die();
}

pub fn find_word(self: *const Forth, name: []const Char) ?*DictEntry {
    var curr = self.dict;
    while (curr) |dict_word| : (curr = dict_word.prev) {
        const word_name = std.mem.span(@as([*:0]u8, @ptrCast(&dict_word.name)));
        if (!dict_word.flags.hidden and std.mem.eql(u8, name, word_name))
            return curr;
    }
    return null;
}

pub fn add_word(
    self: *Forth,
    name: []const Char,
    codeword: Codeword,
    flags: DictEntry.Flags,
) void {
    self.here = std.mem.alignPointer(self.here, @alignOf(DictEntry)).?;
    var dict_entry: *DictEntry = @ptrCast(@alignCast(self.here));
    if (name.len > 32) @panic("Word name too long");
    self.here += @sizeOf(DictEntry);

    dict_entry.prev = self.dict;
    @memcpy(dict_entry.name[0..name.len], name);
    dict_entry.name[name.len] = 0;
    dict_entry.flags = flags;
    dict_entry.codeword = codeword;

    self.dict = dict_entry;
}

fn compile_word(
    self: *Forth,
    name: []const Char,
    flags: DictEntry.Flags,
    comptime code: []const union(enum) {
        word: []const Char,
        byte: Byte,
        cell: Cell,
        string: []const u8,
    },
) void {
    self.add_word(name, docol, flags);
    inline for (code) |item| {
        switch (item) {
            .word => |word_name| {
                const dict_word =
                    self.find_word(word_name) orelse
                    @panic("Word " ++ word_name ++ " not found");
                self.compile_cell(@bitCast(@intFromPtr(&dict_word.codeword)));
            },
            .cell => |cell| self.compile_cell(cell),
            .byte => |byte| self.compile(byte),
            .string => |string| {
                self.compile_cell(string.len);
                @memcpy(self.here, string);
                self.here +=
                    (string.len + cell_size - 1) / cell_size * cell_size;
            },
        }
    }
}

pub inline fn next(self: *Forth) noreturn {
    self.next_word = self.ip[0];
    self.ip += 1;
    @call(.always_tail, self.next_word[0], .{self});
}

pub fn docol(self: *Forth) noreturn {
    self.rpush(@intFromPtr(self.ip));
    self.ip = @ptrCast(@alignCast(&self.next_word[1]));
    self.next();
}

pub fn dodoes(self: *Forth) noreturn {
    self.rpush(@intFromPtr(self.ip));
    self.pushu(@intFromPtr(&self.next_word[1]));

    const dict_entry = @fieldParentPtr(
        DictEntry,
        "codeword",
        &self.next_word[0],
    );

    const code_offset = @divExact(dict_entry.flags.code_offset, cell_size);
    if (code_offset >= 0)
        self.ip = @ptrCast(@alignCast(
            self.next_word + std.math.absCast(code_offset),
        ))
    else
        self.ip = @ptrCast(@alignCast(
            self.next_word - std.math.absCast(code_offset),
        ));
    self.next();
}

pub fn docreate(self: *Forth) noreturn {
    self.pushu(@intFromPtr(&self.next_word[1]));
    self.next();
}

pub fn push(self: *Forth, val: Cell) void {
    self.sp[0] = val;
    self.sp += 1;
}

pub fn pop(self: *Forth) Cell {
    if (self.sp == &self.stack) {
        self.die("stack underflow");
    }
    self.sp -= 1;
    return self.sp[0];
}

pub fn pushu(self: *Forth, val: UCell) void {
    self.push(@bitCast(val));
}

pub fn popu(self: *Forth) UCell {
    return @bitCast(self.pop());
}

pub fn pushd(self: *Forth, val: DCell) void {
    self.pushud(@bitCast(val));
}

pub fn popd(self: *Forth) DCell {
    return @bitCast(self.popud());
}

pub fn pushud(self: *Forth, val: UDCell) void {
    const lo = @as(UCell, @truncate(val));
    const hi = @as(UCell, @truncate(val >> cell_bits));
    self.pushu(lo);
    self.pushu(hi);
}

pub fn popud(self: *Forth) UDCell {
    const hi = @as(UDCell, self.popu());
    const lo = @as(UDCell, self.popu());
    return (hi << cell_bits) | lo;
}

pub fn rpush(self: *Forth, val: UCell) void {
    self.rsp[0] = val;
    self.rsp += 1;
}

pub fn rpop(self: *Forth) UCell {
    if (self.rsp == &self.rstack) {
        self.die("rstack underflow");
    }
    self.rsp -= 1;
    return self.rsp[0];
}

pub fn lpush(self: *Forth, val: UCell) void {
    self.lsp[0] = val;
    self.lsp += 1;
}

pub fn lpop(self: *Forth) UCell {
    if (self.lsp == &self.leavestack) {
        self.die("Leave stack underflow");
    }
    self.lsp -= 1;
    return self.lsp[0];
}

pub fn compile(self: *Forth, val: UByte) void {
    self.here[0] = val;
    self.here += 1;
}

pub fn compile_cell(self: *Forth, val: Cell) void {
    @as(*Cell, @ptrCast(@alignCast(self.here))).* = val;
    self.here += cell_size;
}

pub fn die(self: *Forth, msg: []const u8) noreturn {
    _ = self;
    io.format(null, "\nFatal error: {s}\n", .{msg}) catch {};
    std.process.exit(1);
}

pub fn bye(self: *Forth) noreturn {
    io.format(null, "\nGoodbye.\n", .{}) catch self.die("Could not write to stdout");
    std.process.exit(0);
}

pub fn push_input_source(self: *Forth) void {
    self.input_stack.append(
        self.save_input_source(),
    ) catch self.die("Input stack overflow");
}

pub fn save_input_source(self: *Forth) InputStackItem {
    return .{
        .source = self.input_source,
        .in = self.in,
    };
}

pub fn pop_input_source(self: *Forth) void {
    const source = self.input_stack.pop();
    self.restore_input_source(source);
}

pub fn restore_input_source(self: *Forth, source: InputStackItem) void {
    self.input_source = source.source;
    self.in = source.in;
    switch (self.input_source) {
        .stdin => |*stdin| {
            self.input_buffer = stdin.line[0..stdin.line_len];
        },
        .file => |*file| {
            self.input_buffer = file.line[0..file.line_len];
            io.fileSeek(file.handle, file.line_end) catch
                self.die("Error setting file position");
        },
        .str => |s| self.input_buffer = s,
    }
}
