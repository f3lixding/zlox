const std = @import("std");
const Literal = @import("ast.zig").Literal;
const Expr = @import("ast.zig").Expr;
const Token = @import("token.zig").Token;

pub const InterpreterError = error{
    OperationNotSupported,
};

pub const InterpreterErrorCtx = struct {
    err: InterpreterError,
    expr: *const Expr,
};

pub const Interpreter = struct {
    err: ?InterpreterErrorCtx = null,

    // TODO: enrich the info contained in the error returned by this function
    pub fn reportRunTimeError(self: Interpreter, alloc: std.mem.Allocator) !?[]const u8 {
        if (self.err) |err| {
            const line = err.expr.getLocation().line;
            return try std.fmt.allocPrint(alloc, "Encountered an error on line {d}\n", .{line});
        } else return null;
    }

    // In the book (the java section), the return of evaluate is an Object. This is because the interpreter's design
    // is to return an unified type and during runtime, the interpreter will sort out what type the Object actually
    // is with runtime reflection (i.e. use of instanceof).
    // This is not idiomatically Zig (and I also don't know if it's even possible in Zig). Thus I am opting for an
    // error union of Literal.
    // We'll rely on the try operator for early return for faults that belong to the sub level evaluation.
    // Only when we find faults / errors that belong to the current level do we examine what's going on and report
    // the error.
    pub fn evaluate(self: *Interpreter, expr: *const Expr) InterpreterError!Literal {
        return switch (expr.*) {
            .LITERAL => |l| l.@"1",
            .UNARY => |u| {
                switch (u.@"1") {
                    .NEGATIVE => |inner_expr| {
                        const res = try self.evaluate(inner_expr);
                        if (std.meta.activeTag(res) != .NUMBER) {
                            self.err = .{
                                .err = error.OperationNotSupported,
                                .expr = inner_expr,
                            };
                            return error.OperationNotSupported;
                        }
                        return Literal{ .NUMBER = -1 * res.NUMBER };
                    },
                    .NOT => |inner_expr| {
                        const res = try self.evaluate(inner_expr);
                        return switch (res) {
                            .TRUE => Literal{ .FALSE = {} },
                            .FALSE, .NIL => Literal{ .TRUE = {} },
                            else => {
                                self.err = .{
                                    .err = error.OperationNotSupported,
                                    .expr = inner_expr,
                                };
                                return error.OperationNotSupported;
                            },
                        };
                    },
                }
            },
            .BINARY => |*b| {
                const left = try self.evaluate(b.@"1".left);
                const right = try self.evaluate(b.@"1".right);
                const op = &b.@"1".operator;
                switch (op.*) {
                    .DOUBLE_EQUAL => {
                        const left_tag = std.meta.activeTag(left);
                        const right_tag = std.meta.activeTag(right);
                        if (left_tag != right_tag) return Literal{ .FALSE = {} };
                        switch (left_tag) {
                            .NUMBER => {
                                const left_num = left.NUMBER;
                                const right_num = right.NUMBER;
                                if (left_num == right_num) return Literal{ .TRUE = {} };
                                return Literal{ .FALSE = {} };
                            },
                            .STRING => {
                                const left_str = left.STRING;
                                const right_str = right.STRING;
                                if (std.mem.eql(u8, left_str, right_str)) return Literal{ .TRUE = {} };
                                return Literal{ .FALSE = {} };
                            },
                            .TRUE, .FALSE, .NIL => return Literal{ .TRUE = {} },
                        }
                    },
                    .BANG_EQUAL => {
                        const left_tag = std.meta.activeTag(left);
                        const right_tag = std.meta.activeTag(right);
                        if (left_tag != right_tag) return Literal{ .TRUE = {} };
                        switch (left_tag) {
                            .NUMBER => {
                                const left_num = left.NUMBER;
                                const right_num = right.NUMBER;
                                if (left_num != right_num) return Literal{ .TRUE = {} };
                                return Literal{ .FALSE = {} };
                            },
                            .STRING => {
                                const left_str = left.STRING;
                                const right_str = right.STRING;
                                if (std.mem.eql(u8, left_str, right_str)) return Literal{ .FALSE = {} };
                                return Literal{ .TRUE = {} };
                            },
                            .TRUE, .FALSE => return Literal{ .FALSE = {} },
                            .NIL => return Literal{ .TRUE = {} },
                        }
                    },
                    .GREATER => {
                        const left_tag = std.meta.activeTag(left);
                        const right_tag = std.meta.activeTag(right);
                        if (left_tag != right_tag) {
                            self.err = .{
                                .err = error.OperationNotSupported,
                                .expr = expr,
                            };
                            return error.OperationNotSupported;
                        }
                        switch (left_tag) {
                            .NUMBER => {
                                const left_num = left.NUMBER;
                                const right_num = right.NUMBER;
                                if (left_num > right_num) return Literal{ .TRUE = {} };
                                return Literal{ .FALSE = {} };
                            },
                            .STRING, .TRUE, .FALSE, .NIL => {
                                self.err = .{
                                    .err = error.OperationNotSupported,
                                    .expr = expr,
                                };
                                return error.OperationNotSupported;
                            },
                        }
                    },
                    .GREATER_EQUAL => {
                        const left_tag = std.meta.activeTag(left);
                        const right_tag = std.meta.activeTag(right);
                        if (left_tag != right_tag) {
                            self.err = .{
                                .err = error.OperationNotSupported,
                                .expr = expr,
                            };
                            return error.OperationNotSupported;
                        }
                        switch (left_tag) {
                            .NUMBER => {
                                const left_num = left.NUMBER;
                                const right_num = right.NUMBER;
                                if (left_num >= right_num) return Literal{ .TRUE = {} };
                                return Literal{ .FALSE = {} };
                            },
                            .STRING, .TRUE, .FALSE, .NIL => {
                                self.err = .{
                                    .err = error.OperationNotSupported,
                                    .expr = expr,
                                };
                                return error.OperationNotSupported;
                            },
                        }
                    },
                    .LESS => {
                        const left_tag = std.meta.activeTag(left);
                        const right_tag = std.meta.activeTag(right);
                        if (left_tag != right_tag) return error.OperationNotSupported;
                        switch (left_tag) {
                            .NUMBER => {
                                const left_num = left.NUMBER;
                                const right_num = right.NUMBER;
                                if (left_num < right_num) return Literal{ .TRUE = {} };
                                return Literal{ .FALSE = {} };
                            },
                            .STRING, .TRUE, .FALSE, .NIL => {
                                self.err = .{
                                    .err = error.OperationNotSupported,
                                    .expr = expr,
                                };
                                return error.OperationNotSupported;
                            },
                        }
                    },
                    .LESS_EQUAL => {
                        const left_tag = std.meta.activeTag(left);
                        const right_tag = std.meta.activeTag(right);
                        if (left_tag != right_tag) return error.OperationNotSupported;
                        switch (left_tag) {
                            .NUMBER => {
                                const left_num = left.NUMBER;
                                const right_num = right.NUMBER;
                                if (left_num <= right_num) return Literal{ .TRUE = {} };
                                return Literal{ .FALSE = {} };
                            },
                            .STRING, .TRUE, .FALSE, .NIL => {
                                self.err = .{
                                    .err = error.OperationNotSupported,
                                    .expr = expr,
                                };
                                return error.OperationNotSupported;
                            },
                        }
                    },
                    .MINUS => {
                        const left_tag = std.meta.activeTag(left);
                        const right_tag = std.meta.activeTag(right);
                        if (left_tag != right_tag) return error.OperationNotSupported;
                        switch (left_tag) {
                            .NUMBER => {
                                const left_num = left.NUMBER;
                                const right_num = right.NUMBER;
                                return Literal{ .NUMBER = left_num - right_num };
                            },
                            .STRING, .TRUE, .FALSE, .NIL => {
                                self.err = .{
                                    .err = error.OperationNotSupported,
                                    .expr = expr,
                                };
                                return error.OperationNotSupported;
                            },
                        }
                    },
                    .PLUS => {
                        const left_tag = std.meta.activeTag(left);
                        const right_tag = std.meta.activeTag(right);
                        if (left_tag != right_tag) {
                            self.err = .{
                                .err = error.OperationNotSupported,
                                .expr = expr,
                            };
                            return error.OperationNotSupported;
                        }
                        switch (left_tag) {
                            .NUMBER => {
                                const left_num = left.NUMBER;
                                const right_num = right.NUMBER;
                                return Literal{ .NUMBER = left_num + right_num };
                            },
                            // I had deliberately left out the string concatenation for plus operator
                            // TODO: implement an operator for string concatenation
                            .STRING, .TRUE, .FALSE, .NIL => {
                                self.err = .{
                                    .err = error.OperationNotSupported,
                                    .expr = expr,
                                };
                                return error.OperationNotSupported;
                            },
                        }
                    },
                    .SLASH => {
                        const left_tag = std.meta.activeTag(left);
                        const right_tag = std.meta.activeTag(right);
                        if (left_tag != right_tag) return error.OperationNotSupported;
                        switch (left_tag) {
                            .NUMBER => {
                                const left_num = left.NUMBER;
                                const right_num = right.NUMBER;
                                return Literal{ .NUMBER = left_num / right_num };
                            },
                            .STRING, .TRUE, .FALSE, .NIL => {
                                self.err = .{
                                    .err = error.OperationNotSupported,
                                    .expr = expr,
                                };
                                return error.OperationNotSupported;
                            },
                        }
                    },
                    .STAR => {
                        const left_tag = std.meta.activeTag(left);
                        const right_tag = std.meta.activeTag(right);
                        if (left_tag != right_tag) return error.OperationNotSupported;
                        switch (left_tag) {
                            .NUMBER => {
                                const left_num = left.NUMBER;
                                const right_num = right.NUMBER;
                                return Literal{ .NUMBER = left_num * right_num };
                            },
                            .STRING, .TRUE, .FALSE, .NIL => {
                                self.err = .{
                                    .err = error.OperationNotSupported,
                                    .expr = expr,
                                };
                                return error.OperationNotSupported;
                            },
                        }
                    },
                    .COMMA => return error.OperationNotSupported,
                }
            },
            .GROUPING => |*g| self.evaluate(g.@"1".expr),
            .TERNARY => |t| {
                const cond = t.@"1".cond;
                const pos = t.@"1".pos;
                const neg = t.@"1".neg;
                const cond_res = try self.evaluate(cond);
                return if (isTruthy(cond_res)) try self.evaluate(pos) else try self.evaluate(neg);
            },
        };
    }

    fn isTruthy(literal: Literal) bool {
        return switch (literal) {
            .TRUE => true,
            .FALSE, .NIL => false,
            else => true,
        };
    }
};

test "truthy falsey" {
    const literal_true = Literal{ .TRUE = {} };
    std.debug.assert(Interpreter.isTruthy(literal_true));
}

test "evaluate binary" {
    // Evaluating just the literal should return the literal
    const literal_string = Literal{ .STRING = "hello" };
    var string_expr = Expr{ .LITERAL = .{ .{}, literal_string } };
    var interpreter = Interpreter{};
    var eval_res = try interpreter.evaluate(&string_expr);
    std.debug.assert(std.mem.eql(u8, eval_res.STRING, "hello"));
    // Evaluating a binary expr that compares two things that are the same
    const literal_hello = Literal{ .STRING = "hello" };
    var hello_expr = Expr{ .LITERAL = .{ .{}, literal_hello } };
    var eql_eql_str = Expr{ .BINARY = .{ .{}, .{
        .left = &string_expr,
        .operator = .DOUBLE_EQUAL,
        .right = &hello_expr,
    } } };
    eval_res = try interpreter.evaluate(&eql_eql_str);
    std.debug.assert(std.meta.activeTag(eval_res) == .TRUE);
    // Evaluating a binary expr that compares two thigns that are not the same
    const literal_false = Literal{ .FALSE = {} };
    var false_expr = Expr{ .LITERAL = .{ .{}, literal_false } };
    var false_binary_expr = Expr{ .BINARY = .{ .{}, .{
        .left = &eql_eql_str,
        .operator = .DOUBLE_EQUAL,
        .right = &false_expr,
    } } };
    eval_res = try interpreter.evaluate(&false_binary_expr);
    std.debug.assert(std.meta.activeTag(eval_res) == .FALSE);
    // Evaluating a binary expr that compares two things that are not of the same type
    var num_expr = Expr{ .LITERAL = .{ .{}, .{ .NUMBER = 3.2 } } };
    var str_num_bin_expr = Expr{ .BINARY = .{ .{}, .{
        .left = &hello_expr,
        .operator = .GREATER,
        .right = &num_expr,
    } } };
    const eval_res_two = interpreter.evaluate(&str_num_bin_expr);
    std.debug.assert(std.meta.isError(eval_res_two));
}

test "evaluate unary" {
    var literal_number_expr = Expr{ .LITERAL = .{ .{}, Literal{ .NUMBER = 3.4 } } };
    const neg_number_expr = Expr{ .UNARY = .{
        .{},
        .{ .NEGATIVE = &literal_number_expr },
    } };
    var interpreter = Interpreter{};
    var eval_res = try interpreter.evaluate(&neg_number_expr);
    std.debug.assert(std.meta.activeTag(eval_res) == .NUMBER);
    std.debug.assert(eval_res.NUMBER == -3.4);
    var literal_true_expr = Expr{ .LITERAL = .{ .{}, Literal{ .TRUE = {} } } };
    const not_expr = Expr{ .UNARY = .{
        .{},
        .{ .NOT = &literal_true_expr },
    } };
    eval_res = try interpreter.evaluate(&not_expr);
    std.debug.assert(std.meta.activeTag(eval_res) == .FALSE);
    var literal_str_expr = Expr{ .LITERAL = .{ .{}, Literal{ .STRING = "hello" } } };
    const neg_str_expr = Expr{ .UNARY = .{
        .{},
        .{ .NEGATIVE = &literal_str_expr },
    } };
    const eval_res_two = interpreter.evaluate(&neg_str_expr);
    std.debug.assert(std.meta.isError(eval_res_two));
}

test "evaluate ternary" {
    const Operator = @import("ast.zig").Operator;
    var tern_left_expr = Expr{ .LITERAL = .{ .{}, Literal{ .NUMBER = 1.0 } } };
    var tern_right_expr = Expr{ .LITERAL = .{ .{}, Literal{ .NUMBER = 2.0 } } };
    var tern_cond_bin_left = Expr{ .LITERAL = .{ .{}, Literal{ .NUMBER = 3.3 } } };
    var tern_cond_bin_right = Expr{ .LITERAL = .{ .{}, Literal{ .NUMBER = 3.2 } } };
    const tern_cond_bin_op = Operator{ .DOUBLE_EQUAL = {} };
    var tern_cond_expr = Expr{ .BINARY = .{ .{}, .{
        .left = &tern_cond_bin_left,
        .operator = tern_cond_bin_op,
        .right = &tern_cond_bin_right,
    } } };
    const tern_expr = Expr{ .TERNARY = .{ .{}, .{
        .pos = &tern_left_expr,
        .neg = &tern_right_expr,
        .cond = &tern_cond_expr,
    } } };
    var interpreter = Interpreter{};
    const eval_res = try interpreter.evaluate(&tern_expr);
    std.debug.assert(eval_res.NUMBER == 2.0);
}

test "evaluate group" {
    var literal_num_expr_one = Expr{ .LITERAL = .{ .{}, Literal{ .NUMBER = 3.4 } } };
    var literal_num_expr_two = Expr{ .LITERAL = .{ .{}, Literal{ .NUMBER = 3.4 } } };
    var addition_expr = Expr{ .BINARY = .{ .{}, .{
        .left = &literal_num_expr_one,
        .operator = .PLUS,
        .right = &literal_num_expr_two,
    } } };
    var group_expr = Expr{ .GROUPING = .{ .{}, .{ .expr = &addition_expr } } };
    var literal_num_expr_three = Expr{ .LITERAL = .{ .{}, Literal{ .NUMBER = 2.0 } } };
    var division_expr = Expr{ .BINARY = .{ .{}, .{
        .left = &group_expr,
        .operator = .SLASH,
        .right = &literal_num_expr_three,
    } } };
    var interpreter = Interpreter{};
    const eval_res = try interpreter.evaluate(&division_expr);
    std.debug.assert(eval_res.NUMBER == 3.4);
}

test "error reporting" {
    var literal_num_expr_one = Expr{ .LITERAL = .{ .{}, Literal{ .STRING = "hello" } } };
    var literal_num_expr_two = Expr{ .LITERAL = .{ .{}, Literal{ .NUMBER = 3.4 } } };
    var addition_expr = Expr{ .BINARY = .{ .{
        .line = 10,
    }, .{
        .left = &literal_num_expr_one,
        .operator = .PLUS,
        .right = &literal_num_expr_two,
    } } };
    var group_expr = Expr{ .GROUPING = .{ .{}, .{ .expr = &addition_expr } } };
    var literal_num_expr_three = Expr{ .LITERAL = .{ .{}, Literal{ .NUMBER = 2.0 } } };
    var division_expr = Expr{ .BINARY = .{ .{}, .{
        .left = &group_expr,
        .operator = .SLASH,
        .right = &literal_num_expr_three,
    } } };
    var interpreter = Interpreter{};
    const eval_res = interpreter.evaluate(&division_expr);
    std.debug.assert(std.meta.isError(eval_res));
    const err = interpreter.err;
    std.debug.assert(err != null);
    const err_ctx = err.?;
    std.debug.assert(err_ctx.err == error.OperationNotSupported);
    std.debug.assert(err_ctx.expr.BINARY.@"0".line == 10);
    const alloc = std.testing.allocator;
    const formatted_err = try interpreter.reportRunTimeError(alloc);
    if (formatted_err) |formatted_error| {
        defer alloc.free(formatted_error);
        std.debug.assert(std.mem.eql(u8, formatted_err.?, "Encountered an error on line 10\n"));
    } else unreachable;
}
