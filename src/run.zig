const std = @import("std");

pub fn run(code: []const u8) !void {
    _ = code;
}

pub fn run_file(alloc: std.mem.Allocator, max_bytes: usize, path: []const u8) !void {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    const code = try file.readToEndAlloc(alloc, max_bytes);
    run(code) catch |err| {
        std.debug.print("Error running file {s}: {s}\n", .{ path, err });
    };
}

pub fn run_prompt() !void {
    const stdin = std.io.getStdIn().reader();
    var buffered_reader = std.io.bufferedReader(stdin);
    var in_stream = buffered_reader.reader();

    var buf: [1024]u8 = undefined;
    while (try in_stream.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        std.debug.print("> {s}\n", .{line});
        run(line) catch |err| {
            std.debug.print("Error running line {s}: {s}\n", .{ line, err });
        };
    }
}
