const std = @import("std");
const run_prompt = @import("run.zig").run_prompt;

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
}
