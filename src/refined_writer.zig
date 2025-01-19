const std = @import("std");

// This is an interface that is to be used by the Interrpreter for print statements.
// I want to make it so that the interpreter is able to use different implementations of a writer.
// This is mainly because of testing purposes. I want to be able to test the interpreter with a mock writer (which is normally a file writer that writes to stdout).
//
// On a high level, I want this interface to achieve the following:
// - hide the writing behavior from the Interpreter associated.
// - not have any type specific information in the interface.
// The only downside is that this interface cannot be generic over the type of things to be written.
// This is because I do not wish the generalness to be leaked to the type that depends on this interface.
pub const RefinedWriter = struct {
    // This is the context pointer, where it writes to.
    ptr: *anyopaque,
    writeAllFn: *const fn (ptr: *anyopaque, bytes: []const u8) anyerror!void,

    pub fn init(ptr: anytype) RefinedWriter {
        const T = @TypeOf(ptr);
        std.debug.assert(@hasDecl(@TypeOf(ptr.*), "writeAll"));
        const ptr_info = @typeInfo(T);

        const generated_writeAllFn = struct {
            fn writeAllFn(pointer: *anyopaque, bytes: []const u8) anyerror!void {
                const self: T = @ptrCast(@alignCast(pointer));
                return ptr_info.Pointer.child.writeAll(self, bytes);
            }
        }.writeAllFn;
        return .{
            .ptr = ptr,
            .writeAllFn = generated_writeAllFn,
        };
    }

    pub fn writeAll(self: *RefinedWriter, bytes: []const u8) anyerror!void {
        return self.writeAllFn(self.ptr, bytes);
    }
};

test "writer interface" {
    const TEST_STR = "this is a test string";
    const Storage = struct {
        alloc: std.mem.Allocator,
        storage: std.ArrayList(u8),

        pub fn init(alloc: std.mem.Allocator) @This() {
            return .{
                .alloc = alloc,
                .storage = std.ArrayList(u8).init(alloc),
            };
        }
        pub fn deinit(self: @This()) void {
            self.storage.deinit();
        }
        pub fn writeAll(self: *@This(), bytes: []const u8) anyerror!void {
            try self.storage.appendSlice(bytes);
        }
        pub fn writer(self: *@This()) RefinedWriter {
            return RefinedWriter.init(self);
        }
    };
    var storage = Storage.init(std.testing.allocator);
    defer storage.deinit();
    var writer = storage.writer();
    try writer.writeAll(TEST_STR);
    std.debug.assert(std.mem.eql(u8, TEST_STR, storage.storage.items));
}
