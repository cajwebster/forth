const Forth = @import("../../Forth.zig");

const DictEntry = Forth.DictEntry;
const Codeword = Forth.Codeword;

const f_bool = Forth.f_bool;

/// ( "<spaces>name" -- )
/// Parses a name delimited by spaces, then creates a new dictionary entry for
/// that name. When name is executed, the current ip will be pushed to the
/// return stack, and the vm will start executing the xts that are compiled
/// immediately after the word was created.
pub fn @"DOCOL:"(forth: *Forth) callconv(.C) noreturn {
    const name = forth.word(' ', .skip_leading);
    forth.add_word(name, Forth.docol, .{});
    @call(.always_tail, Forth.next, .{forth});
}

/// ( -- )
/// Same as DOCOL:, but doesn't parse anything and creates a dictionary entry
/// with no name.
pub fn @"NONAME-DOCOL:"(forth: *Forth) callconv(.C) noreturn {
    forth.add_word("", Forth.docol, .{});
    @call(.always_tail, Forth.next, .{forth});
}

/// ( "<spaces>name" -- )
/// Parses a name delimited by spaces, then creates a new dictionary entry for
/// that name. When name is executed, the name's data field addres will be
/// pushed to the stack
pub fn CREATE(forth: *Forth) callconv(.C) noreturn {
    const name = forth.word(' ', .skip_leading);
    forth.add_word(name, Forth.docreate, .{});
    @call(.always_tail, Forth.next, .{forth});
}

/// ( -- )
/// Make the most recent dictionary entry an immediate word
pub fn IMMEDIATE(forth: *Forth) callconv(.C) noreturn {
    forth.dict.?.flags.immediate = true;
    @call(.always_tail, Forth.next, .{forth});
}

/// ( a-addr -- flag )
/// flag is true if the dictionary entry pointed to by addr is an immediate
/// word.
pub fn @"IMMEDIATE?"(forth: *Forth) callconv(.C) noreturn {
    const dict_entry = @as(*DictEntry, @ptrFromInt(forth.popu()));
    forth.push(f_bool(dict_entry.flags.immediate));
    @call(.always_tail, Forth.next, .{forth});
}

/// ( -- )
/// Toggles the hidden flag for the most recent dictionary entry
pub fn HIDDEN(forth: *Forth) callconv(.C) noreturn {
    forth.dict.?.flags.hidden = !forth.dict.?.flags.hidden;
    @call(.always_tail, Forth.next, .{forth});
}

/// ( c-addr -- c-addr 0 | xt 1 | xt -1 )
/// c-addr is a counted string. Looks up a name in the dictionary and returns
/// 0 if the word isn't found, 1 if its immediate and -1 if its not.
pub fn FIND(forth: *Forth) callconv(.C) noreturn {
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
    @call(.always_tail, Forth.next, .{forth});
}

pub fn @">CFA"(forth: *Forth) callconv(.C) noreturn {
    const addr = @as(*DictEntry, @ptrFromInt(forth.popu()));
    forth.pushu(@intFromPtr(&addr.codeword));
    @call(.always_tail, Forth.next, .{forth});
}

pub fn @"CFA>"(forth: *Forth) callconv(.C) noreturn {
    const addr = @as(*Codeword, @ptrFromInt(forth.popu()));
    const dict_entry = @as(*DictEntry, @fieldParentPtr("codeword", addr));
    forth.pushu(@intFromPtr(dict_entry));
    @call(.always_tail, Forth.next, .{forth});
}

pub fn @">BODY"(forth: *Forth) callconv(.C) noreturn {
    const addr = @as(*const Codeword, @ptrFromInt(forth.popu()));
    const dict_entry = @as(*const DictEntry, @fieldParentPtr("codeword", addr));
    forth.pushu(@intFromPtr(dict_entry) + @sizeOf(DictEntry));
    @call(.always_tail, Forth.next, .{forth});
}

pub fn @">DFA"(forth: *Forth) callconv(.C) noreturn {
    const addr = forth.popu();
    forth.pushu(addr + @sizeOf(DictEntry));
    @call(.always_tail, Forth.next, .{forth});
}
