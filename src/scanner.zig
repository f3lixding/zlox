const std = @import("std");
const Token = @import("token.zig").Token;

pub const Scanner = struct {
    start: usize = 0,
    current: usize = 0,
    line: usize = 0,
    tokens: std.ArrayList(Token) = undefined,
    // This is a managed struct
    alloc: std.mem.Allocator,
    src_buf: ?[]const u8 = null,

    pub fn scanTokens(self: *Scanner) void {
        _ = self;
    }

    pub fn init(self: *Scanner, src_path: []const u8) !void {
        self.tokens = std.ArrayList(Token).init(self.alloc);
        self.src_buf = try std.fs.cwd().readFileAlloc(self.alloc, src_path, 100000);
    }

    pub fn deinit(self: *Scanner) void {
        self.tokens.deinit();
        if (self.src_buf) |buf| self.alloc.free(buf);
    }
};

test "initialization test" {
    var scanner = Scanner{
        .alloc = std.testing.allocator,
    };
    scanner.init("test_src.lox") catch unreachable;
    defer scanner.deinit();

    std.debug.assert(scanner.src_buf.?.len > 0);
}
