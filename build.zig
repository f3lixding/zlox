const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "zlox",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    const exe_unit_tests = b.addTest(.{
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });
    const src_dir = std.fs.cwd().openDir("src", .{ .iterate = true }) catch unreachable;
    var walker = src_dir.walk(b.allocator) catch unreachable;
    defer walker.deinit();
    while (walker.next() catch unreachable) |entry| {
        if (std.mem.eql(u8, std.fs.path.extension(entry.basename), ".zig")) {
            const unit_test = b.addTest(.{
                .root_source_file = b.path(b.pathJoin(&.{ "src", entry.path })),
                .target = target,
                .optimize = optimize,
            });
            const run_unit_test = b.addRunArtifact(unit_test);
            exe_unit_tests.step.dependOn(&run_unit_test.step);
        }
    }
    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);
    b.getInstallStep().dependOn(&run_exe_unit_tests.step);
}
