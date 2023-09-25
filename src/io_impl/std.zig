const std = @import("std");

const Forth = @import("../Forth.zig");

pub const FileId = Forth.Cell;
const FileMode = @import("../io.zig").FileMode;
const File = std.fs.File;

pub fn readChar() !Forth.Char {
    return std.io.getStdIn().reader().readByte();
}

pub fn readLine(file: ?FileId, line: []Forth.Char) ![]u8 {
    var buf = std.io.fixedBufferStream(line);
    const reader = if (file) |f| (try fileFromId(f)).reader() else std.io.getStdIn().reader();
    reader.streamUntilDelimiter(
        buf.writer(),
        '\n',
        line.len,
    ) catch |e| switch (e) {
        error.StreamTooLong => {},
        error.EndOfStream => return if (buf.getWritten().len > 0)
            buf.getWritten()
        else
            e,
        else => return e,
    };
    return buf.getWritten();
}

pub fn format(fileid: ?FileId, comptime fmt: []const u8, args: anytype) !void {
    const file = if (fileid) |id| try fileFromId(id) else std.io.getStdOut();
    const writer = file.writer();
    return std.fmt.format(writer, fmt, args);
}

pub fn openFile(path: []const u8, mode: FileMode) !FileId {
    const options = openOptions(mode);
    return if (std.fs.path.isAbsolute(path))
        fileId(try std.fs.openFileAbsolute(path, options))
    else
        fileId(try std.fs.cwd().openFile(path, options));
}

pub fn createFile(path: []const u8, mode: FileMode) !FileId {
    const options = createOptions(mode);
    return if (std.fs.path.isAbsolute(path))
        fileId(try std.fs.createFileAbsolute(path, options))
    else
        fileId(try std.fs.cwd().createFile(path, options));
}

pub fn deleteFile(path: []const u8) !void {
    return if (std.fs.path.isAbsolute(path))
        std.fs.deleteFileAbsolute(path)
    else
        std.fs.cwd().deleteFile(path);
}

pub fn renameFile(old_path: []const u8, new_path: []const u8) !void {
    const old_dir = if (std.fs.path.isAbsolute(old_path))
        try std.fs.openDirAbsolute("/", .{})
    else
        std.fs.cwd();
    const new_dir = if (std.fs.path.isAbsolute(new_path))
        try std.fs.openDirAbsolute("/", .{})
    else
        std.fs.cwd();
    return std.fs.rename(old_dir, old_path, new_dir, new_path);
}

pub fn fileStatus(path: []const u8) !void {
    return if (std.fs.path.isAbsolute(path))
        std.fs.accessAbsolute(path, .{})
    else
        std.fs.cwd().access(path, .{});
}

pub fn readFile(fileid: FileId, buf: []u8) !Forth.UCell {
    const file = try fileFromId(fileid);
    return @intCast(try file.read(buf));
}

pub fn flushFile(fileid: FileId) !void {
    return (try fileFromId(fileid)).sync();
}

pub fn closeFile(file: FileId) !void {
    (try fileFromId(file)).close();
}

pub fn filePosition(fileid: FileId) !Forth.UDCell {
    const file = try fileFromId(fileid);
    return @intCast(try file.getPos());
}

pub fn resizeFile(fileid: FileId, length: Forth.UDCell) !void {
    const file = try fileFromId(fileid);
    return file.setEndPos(@intCast(length));
}

pub fn fileSize(fileid: FileId) !Forth.UDCell {
    const file = try fileFromId(fileid);
    return @intCast(try file.getEndPos());
}

pub fn fileSeek(fileid: FileId, pos: Forth.UDCell) !void {
    const file = try fileFromId(fileid);
    try file.seekTo(@intCast(pos));
}

pub fn atEnd(fileid: FileId) !bool {
    return try filePosition(fileid) == try fileSize(fileid);
}

fn openOptions(mode: FileMode) File.OpenFlags {
    return .{
        .mode = switch (mode) {
            .ro => .read_only,
            .wo => .write_only,
            .rw => .read_write,
        },
    };
}

fn createOptions(mode: FileMode) File.CreateFlags {
    return .{
        .read = switch (mode) {
            .ro, .rw => true,
            .wo => false,
        },
        .truncate = true,
    };
}

fn fileId(file: File) FileId {
    return @intCast(file.handle);
}

fn fileFromId(fileid: FileId) !File {
    if (fileid <= 0) return error.InvalidFileid;
    return File{ .handle = @intCast(fileid) };
}
