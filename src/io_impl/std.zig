const std = @import("std");

const Forth = @import("../Forth.zig");

pub const File = std.fs.File;

pub fn fileId(file: File) Forth.Cell {
    return @intCast(file.handle);
}

pub fn readChar() !Forth.Char {
    return std.io.getStdIn().reader().readByte();
}

pub fn readLine(file: ?File, line: *[Forth.max_line_len]Forth.Char) ![]u8 {
    var buf = std.io.fixedBufferStream(line);
    const reader = if (file) |f| f.reader() else std.io.getStdIn().reader();
    reader.streamUntilDelimiter(
        buf.writer(),
        '\n',
        line.len,
    ) catch |err| switch (err) {
        error.StreamTooLong => reader.skipUntilDelimiterOrEof('\n') catch {},
        else => return err,
    };
    return buf.getWritten();
}

pub fn format(comptime fmt: []const u8, args: anytype) !void {
    const writer = std.io.getStdOut().writer();
    return std.fmt.format(writer, fmt, args);
}

pub fn openFile(path: []const u8) !File {
    return if (std.fs.path.isAbsolute(path))
        std.fs.openFileAbsolute(path, .{})
    else
        std.fs.cwd().openFile(path, .{});
}
