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
                    .NUMBER => |num| return try std.fmt.allocPrint(alloc, "{d}", .{num}),
                    .STRING => |str| return try std.fmt.allocPrint(alloc, "\"{s}\"", .{str}),
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
                        return try std.fmt.allocPrint(alloc, "(- {s})", .{child});
                    },
                    .NOT => |inner| {
                        const child = try inner.getPrintableRepr(alloc);
                        defer alloc.free(child);
                        return std.fmt.allocPrint(alloc, "(! {s})", .{child});
                    },
                }
            },
            .BINARY => |binary| {
                const left_output = try binary.left.getPrintableRepr(alloc);
                defer alloc.free(left_output);
                const right_output = try binary.right.getPrintableRepr(alloc);
                defer alloc.free(right_output);
                switch (binary.operator) {
                    .DOUBLE_EQUAL => return try std.fmt.allocPrint(alloc, "({s} == {s})", .{ left_output, right_output }),
                    .BANG_EQUAL => return try std.fmt.allocPrint(alloc, "({s} != {s})", .{ left_output, right_output }),
                    .GREATER => return try std.fmt.allocPrint(alloc, "({s} > {s})", .{ left_output, right_output }),
                    .GREATER_EQUAL => return try std.fmt.allocPrint(alloc, "({s} >= {s})", .{ left_output, right_output }),
                    .LESS => return try std.fmt.allocPrint(alloc, "({s} < {s})", .{ left_output, right_output }),
                    .LESS_EQUAL => return try std.fmt.allocPrint(alloc, "({s} <= {s})", .{ left_output, right_output }),
                    .MINUS => return try std.fmt.allocPrint(alloc, "({s} - {s})", .{ left_output, right_output }),
                    .PLUS => return try std.fmt.allocPrint(alloc, "({s} + {s})", .{ left_output, right_output }),
                    .SLASH => return try std.fmt.allocPrint(alloc, "({s} / {s})", .{ left_output, right_output }),
                    .STAR => return try std.fmt.allocPrint(alloc, "({s} * {s})", .{ left_output, right_output }),
                    .COMMA => return try std.fmt.allocPrint(alloc, "({s}, {s})", .{ left_output, right_output }),
                }
            },
            .GROUPING => |grouping| {
                const child = try grouping.expr.getPrintableRepr(alloc);
                defer alloc.free(child);
                return try std.fmt.allocPrint(alloc, "({s})", .{child});
            },
        };
    }

    pub fn print(self: Expr, alloc: std.mem.Allocator) !void {
        const repr = try self.getPrintableRepr(alloc);
        defer alloc.free(repr);
        std.debug.print("{s}\n", .{repr});
    }
};

pub const Literal = union(enum) { NUMBER: f64, STRING: []const u8, TRUE, FALSE, NIL };

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

test "pretty print test" {
    // Testing for a simple unary expression
    const literal_true = Literal{ .TRUE = {} };
    var literal_true_expr = Expr{ .LITERAL = literal_true };
    const unary_not_true = Unary{ .NOT = &literal_true_expr };
    const expr_unary = Expr{ .UNARY = unary_not_true };
    const print_from_expr_unary = try expr_unary.getPrintableRepr(std.testing.allocator);
    defer std.testing.allocator.free(print_from_expr_unary);
    std.debug.assert(std.mem.eql(u8, print_from_expr_unary, "(! true)"));

    // Testing for a simple binary expression
    const literal_1 = Literal{ .NUMBER = 1.1 };
    var literal_1_expr = Expr{ .LITERAL = literal_1 };
    const literal_2 = Literal{ .NUMBER = 2.1 };
    var literal_2_expr = Expr{ .LITERAL = literal_2 };
    const binary_plus = Binary{ .left = &literal_1_expr, .operator = .PLUS, .right = &literal_2_expr };
    const expr_binary = Expr{ .BINARY = binary_plus };
    const print_from_expr_binary = try expr_binary.getPrintableRepr(std.testing.allocator);
    defer std.testing.allocator.free(print_from_expr_binary);
    std.debug.assert(std.mem.eql(u8, print_from_expr_binary, "(1.1 + 2.1)"));

    // Testing for a simple grouping expression
    const literal_3 = Literal{ .NUMBER = 3.0 };
    const literal_4 = Literal{ .NUMBER = 4.0 };
    var expr_literal_3 = Expr{ .LITERAL = literal_3 };
    var expr_literal_4 = Expr{ .LITERAL = literal_4 };
    const binary_plus_3_4 = Binary{ .left = &expr_literal_3, .operator = .PLUS, .right = &expr_literal_4 };
    var expr_binary_plus_3_4 = Expr{ .BINARY = binary_plus_3_4 };
    const grouping = Grouping{ .expr = &expr_binary_plus_3_4 };
    var expr_grouping = Expr{ .GROUPING = grouping };
    const binary_grouping_5 = Binary{ .left = &expr_literal_3, .operator = .STAR, .right = &expr_grouping };
    const expr_grouping_5 = Expr{ .BINARY = binary_grouping_5 };
    const print_from_expr_grouping = try expr_grouping_5.getPrintableRepr(std.testing.allocator);
    defer std.testing.allocator.free(print_from_expr_grouping);
    std.debug.assert(std.mem.eql(u8, print_from_expr_grouping, "(3 * ((3 + 4)))"));
}
