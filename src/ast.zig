const std = @import("std");

pub const Expr = union(enum) {
    LITERAL: Literal,
    UNARY: Unary,
    BINARY: Binary,
    GROUPING: Grouping,

    pub fn getPrintableRepr(self: Expr, alloc: std.mem.Allocator) ![]const u8 {
        return switch (self) {
            .LITERAL => |literal| {
                switch (literal) {
                    .NUMBER => |num| return try std.fmt.allocPrint(alloc, "{any}", .{num}),
                    .STRING => |str| return try std.fmt.allocPrint(alloc, "\"{any}\"", .{str}),
                    .TRUE => return try std.fmt.allocPrint(alloc, "true", .{}),
                    .FALSE => return try std.fmt.allocPrint(alloc, "false", .{}),
                    .NIL => return try std.fmt.allocPrint(alloc, "nil", .{}),
                }
            },
            .UNARY => |unary| {
                switch (unary) {
                    .NEGATIVE => |inner| {
                        const child = try inner.getPrintableRepr(alloc);
                        defer alloc.free(child);
                        return try std.fmt.allocPrint(alloc, "(- {any})", .{child});
                    },
                    .NOT => |inner| {
                        const child = try inner.getPrintableRepr(alloc);
                        defer alloc.free(child);
                        return std.fmt.allocPrint(alloc, "(! {any})", .{child});
                    },
                }
            },
            .BINARY => |binary| {
                const left_output = try binary.left.getPrintableRepr(alloc);
                defer alloc.free(left_output);
                const right_output = try binary.right.getPrintableRepr(alloc);
                defer alloc.free(right_output);
                switch (binary.operator) {
                    .DOUBLE_EQUAL => return try std.fmt.allocPrint(alloc, "({any} == {any})", .{ left_output, right_output }),
                    .BANG_EQUAL => return try std.fmt.allocPrint(alloc, "({any} != {any})", .{ left_output, right_output }),
                    .GREATER => return try std.fmt.allocPrint(alloc, "({any} > {any})", .{ left_output, right_output }),
                    .GREATER_EQUAL => return try std.fmt.allocPrint(alloc, "({any} >= {any})", .{ left_output, right_output }),
                    .LESS => return try std.fmt.allocPrint(alloc, "({any} < {any})", .{ left_output, right_output }),
                    .LESS_EQUAL => return try std.fmt.allocPrint(alloc, "({any} <= {any})", .{ left_output, right_output }),
                    .MINUS => return try std.fmt.allocPrint(alloc, "({any} - {any})", .{ left_output, right_output }),
                    .PLUS => return try std.fmt.allocPrint(alloc, "({any} + {any})", .{ left_output, right_output }),
                }
            },
            .GROUPING => |grouping| {
                const child = try grouping.expr.getPrintableRepr(alloc);
                defer alloc.free(child);
                return try std.fmt.allocPrint(alloc, "({any})", .{child});
            },
        };
    }

    pub fn print(self: Expr, alloc: std.mem.Allocator) void {
        const repr = self.getPrintableRepr(alloc);
        defer alloc.free(repr);
        std.debug.print("{any}\n", .{repr});
    }
};

pub const Literal = union(enum) { NUMBER: f64, STRING: []const u8, TRUE: "true", FALSE: "false", NIL: "nil" };

pub const Grouping = struct {
    expr: *Expr,
};

pub const Unary = union(enum) {
    NEGATIVE: *Expr,
    NOT: *Expr,
};

pub const Binary = struct {
    left: *Expr,
    operator: Operator,
    right: *Expr,
};

pub const Operator = union(enum) {
    DOUBLE_EQUAL,
    BANG_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,
    MINUS,
    PLUS,
    SLASH,
    STAR,
    COMMA,
};
