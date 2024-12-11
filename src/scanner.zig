const std = @import("std");
const Token = @import("token.zig").Token;

pub const ScanError = error{
    OutOfMemory,
    InvalidCharacter,
    IncompleteString,
};

pub const Scanner = struct {
    start: usize = 0,
    current: usize = 0,
    line: usize = 0,
    tokens: std.ArrayList(Token) = undefined,
    // This is a managed struct
    alloc: std.mem.Allocator,
    src_buf: ?[]const u8 = null,

    pub fn init(self: *Scanner, comptime src_path: []const u8, max_bytes: usize) !void {
        self.tokens = std.ArrayList(Token).init(self.alloc);
        if (std.mem.endsWith(u8, src_path, ".lox")) {
            self.src_buf = try std.fs.cwd().readFileAlloc(self.alloc, src_path, max_bytes);
        } else {
            // for testing
            self.src_buf = try self.alloc.dupe(u8, src_path);
        }
    }

    pub fn deinit(self: *Scanner) void {
        self.tokens.deinit();
        if (self.src_buf) |buf| self.alloc.free(buf);
    }

    pub fn isEOF(self: Scanner) bool {
        std.debug.assert(self.src_buf != null);
        return self.current >= self.src_buf.?.len - 1;
    }

    pub fn scanTokens(self: *Scanner) !void {
        const start_idx = self.current;
        const src_buf = self.src_buf.?;
        const cur_char = src_buf[self.current];
        self.current += 1;

        var scan_res: ?ScanError = null;

        // Single character tokens
        scan_res = switch (cur_char) {
            ' ',
            '\n',
            '\r',
            '\t',
            => blk: {
                if (cur_char == '\n') {
                    self.line += 1;
                }
                break :blk null;
            },
            '(' => blk: {
                try self.addToken(Token{ .LEFT_PAREN = .{ .line = self.line } });
                break :blk null;
            },
            ')' => blk: {
                try self.addToken(Token{ .RIGHT_PAREN = .{ .line = self.line } });
                break :blk null;
            },
            '{' => blk: {
                try self.addToken(Token{ .LEFT_BRACE = .{ .line = self.line } });
                break :blk null;
            },
            '}' => blk: {
                try self.addToken(Token{ .RIGHT_BRACE = .{ .line = self.line } });
                break :blk null;
            },
            ',' => blk: {
                try self.addToken(Token{ .COMMA = .{ .line = self.line } });
                break :blk null;
            },
            '.' => blk: {
                try self.addToken(Token{ .DOT = .{ .line = self.line } });
                break :blk null;
            },
            '-' => blk: {
                try self.addToken(Token{ .MINUS = .{ .line = self.line } });
                break :blk null;
            },
            '+' => blk: {
                try self.addToken(Token{ .PLUS = .{ .line = self.line } });
                break :blk null;
            },
            ';' => blk: {
                try self.addToken(Token{ .SEMICOLON = .{ .line = self.line } });
                break :blk null;
            },
            '*' => blk: {
                try self.addToken(Token{ .STAR = .{ .line = self.line } });
                break :blk null;
            },
            else => blk: {
                break :blk error.InvalidCharacter;
            },
        };

        if (scan_res == null) {
            return;
        } else if (scan_res.? != error.InvalidCharacter) {
            return scan_res.?;
        }

        // One or two character tokens
        scan_res = switch (cur_char) {
            '!' => blk: {
                if (src_buf[self.current] == '=') {
                    try self.addToken(Token{ .BANG_EQUAL = .{ .line = self.line } });
                    self.current += 1;
                    break :blk null;
                } else {
                    try self.addToken(Token{ .BANG = .{ .line = self.line } });
                    break :blk null;
                }
            },
            '=' => blk: {
                if (src_buf[self.current] == '=') {
                    try self.addToken(Token{ .EQUAL_EQUAL = .{ .line = self.line } });
                    self.current += 1;
                    break :blk null;
                } else {
                    try self.addToken(Token{ .EQUAL = .{ .line = self.line } });
                    break :blk null;
                }
            },
            '<' => blk: {
                if (src_buf[self.current] == '=') {
                    try self.addToken(Token{ .LESS_EQUAL = .{ .line = self.line } });
                    self.current += 1;
                    break :blk null;
                } else {
                    try self.addToken(Token{ .LESS = .{ .line = self.line } });
                    break :blk null;
                }
            },
            '>' => blk: {
                if (src_buf[self.current] == '=') {
                    try self.addToken(Token{ .GREATER_EQUAL = .{ .line = self.line } });
                    self.current += 1;
                    break :blk null;
                } else {
                    try self.addToken(Token{ .GREATER = .{ .line = self.line } });
                    break :blk null;
                }
            },
            '/' => blk: {
                if (src_buf[self.current] == '/') {
                    self.current += 1;
                    var idx = self.current;
                    while (src_buf[idx] != '\n' and idx < src_buf.len) {
                        idx += 1;
                    }
                    self.current = idx;
                    break :blk null;
                } else {
                    try self.addToken(Token{ .SLASH = .{ .line = self.line } });
                    self.current += 1;
                    break :blk null;
                }
            },
            '"' => blk: {
                var idx = self.current;
                const end_quote_idx = while (idx < src_buf.len) {
                    if (src_buf[idx] == '"') {
                        break idx;
                    }
                    idx += 1;
                } else default: {
                    break :default null;
                } orelse {
                    self.current = idx;
                    break :blk error.IncompleteString;
                };
                try self.addToken(Token{ .STRING = .{ .line = self.line, .lexeme = src_buf[self.current..end_quote_idx] } });
                self.current = end_quote_idx + 1;
                break :blk null;
            },
            // TODO: 
            // - add support for negative nubmers
            '0'...'9' => blk: {
                // We'll need to start at the start index.
                // Keep in mind that we had already advanced current.
                var idx = start_idx;
                const end_idx = while (idx < src_buf.len) {
                    if (
                        ((src_buf[idx] < '0' or src_buf[idx] > '9') and src_buf[idx] != '.') or 
                        (src_buf[idx] == '.' and idx < src_buf.len - 1 and src_buf[idx + 1] < '0' and src_buf[idx + 1] > '9')
                     ) {
                        break idx;
                    }
                    idx += 1;
                } else default: {
                    break :default src_buf.len;
                };
                const num_str = src_buf[start_idx..end_idx];
                try self.addToken(Token{ .NUMBER = .{ .line = self.line, .lexeme = num_str } });
                self.current = end_idx;
                break :blk null;
            },
            else => blk: {
                break :blk error.InvalidCharacter;
            },
        };

        if (scan_res == null) {
            return;
        } else if (scan_res.? != error.InvalidCharacter) {
            return scan_res.?;
        }

        // Literals
        const var_name = self.getNextContiguousBuffer();
        if (self.tokens.getLast() == .VAR) {
            // This is an identifier
            // TODO: check if there has been any errors since the var token
            try self.addToken(Token{ .IDENTIFIER = .{ .line = self.line, .lexeme = var_name } });
            self.current = self.current + var_name.len;
            return;
        } else if (self.isVarNameSeen(var_name)) {
            try self.addToken(Token{ .IDENTIFIER = .{ .line = self.line, .lexeme = var_name } });
            self.current = self.current + var_name.len;
            return;
        }
        // Multi character tokens

        if (scan_res) |err| {
            return err;
        }
    }

    inline fn getNextContiguousBuffer(self: Scanner) []const u8 {
        var current_char = self.src_buf.?[self.current];
        var idx = self.current;
        // exclusive
        const end_of_var_idx = while (current_char != ' ' and idx < self.src_buf.?.len) : ({
            idx += 1;
            current_char = self.src_buf.?[idx];
        }) {
            // Nothing to write here since it's already in the while loop incremental condition
        } else blk: {
            break :blk idx;
        };

        return self.src_buf.?[self.current..end_of_var_idx];
    }

    inline fn isVarNameSeen(self: Scanner, var_name: []const u8) bool {
        _ = self;
        _ = var_name;
        // TODO: implement this
        return false;
    }

    fn addToken(self: *Scanner, token: Token) std.mem.Allocator.Error!void {
        return self.tokens.append(token);
    }
};

test "parse single character token" {
    const src =
        \\+ + +
        \\ - - -
        \\!= != !=
    ;
    var scanner = Scanner{
        .alloc = std.testing.allocator,
    };
    const max_bytes = 10000;
    try scanner.init(src, max_bytes);
    defer scanner.deinit();

    while (true) {
        scanner.scanTokens() catch |err| {
            std.debug.print("Failed to scan tokens: {}\n", .{err});
        };
        if (scanner.isEOF()) break;
    }

    var res_map = std.StringHashMap(i32).init(std.testing.allocator);
    defer res_map.deinit();
    for (scanner.tokens.items) |*token| {
        const lexeme = token.getLexeme() orelse continue;
        const entry = try res_map.getOrPut(lexeme);
        if (entry.found_existing) {
            entry.value_ptr.* += 1;
        } else {
            entry.value_ptr.* = 1;
        }
    }
    var iter = res_map.iterator();
    while (iter.next()) |entry| {
        const count = entry.value_ptr.*;
        std.debug.assert(count == 3);
    }
}

test "parse string token" {
    const src =
        \\ "short string"
        \\ "this is a slightly longer string"
    ;
    var scanner = Scanner{
        .alloc = std.testing.allocator,
    };
    const max_bytes = 10000;
    try scanner.init(src, max_bytes);
    defer scanner.deinit();

    while (true) {
        scanner.scanTokens() catch |err| {
            std.debug.print("Failed to scan tokens: {}\n", .{err});
        };
        if (scanner.isEOF()) break;
    }

    for (scanner.tokens.items) |*token| {
        std.debug.assert(token.* == .STRING);
        const lexeme = token.getLexeme() orelse continue;
        std.debug.assert(std.mem.eql(u8, lexeme, "short string") or std.mem.eql(u8, lexeme, "this is a slightly longer string"));
    }
}

test "parse number token" {
    const src =
        \\ 1
        \\ 1234
        \\ 0.123
        \\ 123.345
    ;
    var scanner = Scanner{
        .alloc = std.testing.allocator,
    };
    const max_bytes = 10000;
    try scanner.init(src, max_bytes);
    defer scanner.deinit();

    while (true) {
        scanner.scanTokens() catch |err| {
            std.debug.print("Failed to scan tokens: {}\n", .{err});
        };
        if (scanner.isEOF()) break;
    }
    for (scanner.tokens.items) |*token| {
        std.debug.assert(token.* == .NUMBER);
        const lexeme = token.getLexeme() orelse continue;
        std.debug.assert(
            std.mem.eql(u8, lexeme, "1") or 
            std.mem.eql(u8, lexeme, "1234") or 
            std.mem.eql(u8, lexeme, "0.123") or 
            std.mem.eql(u8, lexeme, "123.345")
        );
    }
}

test "parse multi character token" {}

test "initialization test" {
    var scanner = Scanner{
        .alloc = std.testing.allocator,
    };
    const max_bytes = 10000;
    scanner.init("test_src.lox", max_bytes) catch unreachable;
    defer scanner.deinit();
    std.debug.assert(scanner.src_buf.?.len > 0);

    while (true) {
        scanner.scanTokens() catch |err| {
            std.debug.print("Failed to scan tokens: {}\n", .{err});
        };
        if (scanner.isEOF()) break;
    }
}
