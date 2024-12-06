const std = @import("std");

pub fn run() !void {}

pub fn run_file(path: []const u8) !void {
    _ = path;
}

pub fn run_prompt() !void {
    const stdin = std.io.getStdIn().reader();
    var buffered_reader = std.io.bufferedReader(stdin);
    var in_stream = buffered_reader.reader();

    var buf: [1024]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        std.debug.print("> {s}\n", .{line});
    }
}
