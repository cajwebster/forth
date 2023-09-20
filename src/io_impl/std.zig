const std = @import("std");

const Forth = @import("../Forth.zig");

pub const FileId = Forth.Cell;
const FileMode = @import("../io.zig").FileMode;
const File = std.fs.File;

pub fn readChar() !Forth.Char {
    return std.io.getStdIn().reader().readByte();
}

pub fn readLine(file: ?FileId, line: *[Forth.max_line_len]Forth.Char) ![]u8 {
    var buf = std.io.fixedBufferStream(line);
    const reader = if (file) |f| (try fileFromId(f)).reader() else std.io.getStdIn().reader();
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

fn fileOptions(mode: FileMode) File.OpenFlags {
    return .{ .mode = switch (mode) {
        .ro => .read_only,
        .wo => .write_only,
        .rw => .read_write,
    } };
}

pub fn openFile(path: []const u8, mode: FileMode) !FileId {
    const options = fileOptions(mode);
    return if (std.fs.path.isAbsolute(path))
        fileId(try std.fs.openFileAbsolute(path, options))
    else
        fileId(try std.fs.cwd().openFile(path, options));
}

fn fileId(file: File) Forth.Cell {
    return @intCast(file.handle);
}

fn fileFromId(fileid: Forth.Cell) !File {
    if (fileid <= 0) return error.InvalidFileid;
    return File{ .handle = @intCast(fileid) };
}
