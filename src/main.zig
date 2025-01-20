const std = @import("std");
const run_prompt = @import("run.zig").run_prompt;
const Parser = @import("parser.zig").Parser;
const Token = @import("token.zig").Token;
const Interpreter = @import("interpreter.zig").Interpreter;
const StdOutWriter = @import("refined_writer.zig").StdOutWriter;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    // DELETE ME
    for (args, 0..) |arg, i| {
        std.debug.print("Arg {d}: {s}\n", .{ i, arg });
    }

    if (args.len < 2) {
        // Here we will enter an interactive shell
        try run_prompt();
    } else {
        // Here we will run the script by interpreting it
        // We will for now only take into consideration the first arg (index 1)
    }

    const tokens = scan_files(allocator, args[1]);
    var parser = Parser.init(allocator);
    const stdout_writer = StdOutWriter{};
    var interpreter = Interpreter.init(stdout_writer, allocator);
    parser.tokens = tokens;
    if (parser.parse()) |stmts| {
        for (stmts.items) |stmt| {
            try interpreter.evaluateStmt(stmt);
        }
    } else |err| {
        std.debug.print("Error: {s}\n", .{err});
    }
}

// TODO: implement
fn scan_files(alloc: std.mem.Allocator, comptime src_path: []const u8) std.ArrayList(Token) {
    _ = alloc;
    _ = src_path;
    return undefined;
}
