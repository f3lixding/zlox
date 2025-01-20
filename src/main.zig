const std = @import("std");
const run_prompt = @import("run.zig").run_prompt;
const Parser = @import("parser.zig").Parser;
const Token = @import("token.zig").Token;
const Interpreter = @import("interpreter.zig").Interpreter;
const StdOutWriter = @import("refined_writer.zig").StdOutWriter;
const Scanner = @import("scanner.zig").Scanner;

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

    var scanner = Scanner{ .alloc = allocator };
    defer scanner.deinit();
    try scanner.init(args[1], 10000);
    while (!scanner.isEOF()) {
        try scanner.scanTokens();
    }
    var parser = Parser.init(allocator);
    defer parser.deinit();
    var stdout_writer = StdOutWriter{};
    const stdout_writer_writer = stdout_writer.writer();
    var interpreter = Interpreter.init(stdout_writer_writer, allocator);
    parser.tokens = scanner.tokens.?;
    scanner.tokens = null;
    const parse_res = parser.parse();
    if (parse_res) |*stmts| {
        defer stmts.deinit();
        defer for (stmts.items) |stmt| {
            stmt.deinit(parser.alloc);
        };
        for (stmts.items) |*stmt| {
            const res = try interpreter.evaluateStmt(stmt);
            if (res) |val| {
                const printable_repr = try val.getPrintableRepr(allocator);
                defer allocator.free(printable_repr);
                std.debug.print("Result: {s}\n", .{printable_repr});
            }
        }
    } else |err| {
        std.debug.print("Error: {!}\n", .{err});
    }
    std.debug.print("Done\n", .{});
}
