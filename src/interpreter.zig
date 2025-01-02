const std = @import("std");
const Literal = @import("ast.zig").Literal;
const Expr = @import("ast.zig").Expr;

pub const InterpreterError = error{
    OperationNotSupported,
};

pub const Interpreter = struct {
    // In the book (the java section), the return of evaluate is an Object. This is because the interpreter's design
    // is to return an unified type and during runtime, the interpreter will sort out what type the Object actually
    // is with runtime reflection (i.e. use of instanceof).
    // This is not idiomatically Zig (and I also don't know if it's even possible in Zig). Thus I am opting for an
    // error union of Literal.
    pub fn evaluate(self: *Interpreter, expr: *const Expr) InterpreterError!Literal {
        return switch (expr.*) {
            .LITERAL => |l| l,
            .UNARY => |u| {
                switch (u) {
                    .NEGATIVE => |inner_expr| {
                        const res = try self.evaluate(inner_expr);
                        if (std.meta.activeTag(res) != .NUMBER) return error.OperationNotSupported;
                        return Literal{ .NUMBER = -1 * res.NUMBER };
                    },
                    .NOT => |inner_expr| {
                        const res = try self.evaluate(inner_expr);
                        return switch (res) {
                            .TRUE => Literal{ .FALSE = {} },
                            .FALSE, .NIL => Literal{ .TRUE = {} },
                            else => error.OperationNotSupported,
                        };
                    },
                }
            },
            .BINARY => |*b| {
                const left = try self.evaluate(b.left);
                const right = try self.evaluate(b.right);
                const op = &b.operator;
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
                            .TRUE, .FALSE => return Literal{ .TRUE = {} },
                            .NIL => return Literal{ .FALSE = {} },
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
                        if (left_tag != right_tag) return error.OperationNotSupported;
                        switch (left_tag) {
                            .NUMBER => {
                                const left_num = left.NUMBER;
                                const right_num = right.NUMBER;
                                if (left_num > right_num) return Literal{ .TRUE = {} };
                                return Literal{ .FALSE = {} };
                            },
                            .STRING, .TRUE, .FALSE, .NIL => return error.OperationNotSupported,
                        }
                    },
                    .GREATER_EQUAL => {
                        const left_tag = std.meta.activeTag(left);
                        const right_tag = std.meta.activeTag(right);
                        if (left_tag != right_tag) return error.OperationNotSupported;
                        switch (left_tag) {
                            .NUMBER => {
                                const left_num = left.NUMBER;
                                const right_num = right.NUMBER;
                                if (left_num >= right_num) return Literal{ .TRUE = {} };
                                return Literal{ .FALSE = {} };
                            },
                            .STRING, .TRUE, .FALSE, .NIL => return error.OperationNotSupported,
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
                            .STRING, .TRUE, .FALSE, .NIL => return error.OperationNotSupported,
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
                            .STRING, .TRUE, .FALSE, .NIL => return error.OperationNotSupported,
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
                            .STRING, .TRUE, .FALSE, .NIL => return error.OperationNotSupported,
                        }
                    },
                    .PLUS => {
                        const left_tag = std.meta.activeTag(left);
                        const right_tag = std.meta.activeTag(right);
                        if (left_tag != right_tag) return error.OperationNotSupported;
                        switch (left_tag) {
                            .NUMBER => {
                                const left_num = left.NUMBER;
                                const right_num = right.NUMBER;
                                return Literal{ .NUMBER = left_num + right_num };
                            },
                            .STRING, .TRUE, .FALSE, .NIL => return error.OperationNotSupported,
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
                            .STRING, .TRUE, .FALSE, .NIL => return error.OperationNotSupported,
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
                            .STRING, .TRUE, .FALSE, .NIL => return error.OperationNotSupported,
                        }
                    },
                    .COMMA => return error.OperationNotSupported,
                }
            },
            .GROUPING => |*g| self.evaluate(g.expr),
            .TERNARY => |t| {
                const cond = t.cond;
                const pos = t.pos;
                const neg = t.neg;
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
    var string_expr = Expr{ .LITERAL = literal_string };
    var interpreter = Interpreter{};
    var eval_res = try interpreter.evaluate(&string_expr);
    std.debug.assert(std.mem.eql(u8, eval_res.STRING, "hello"));
    // Evaluating a binary expr that compares two things that are the same
    const literal_hello = Literal{ .STRING = "hello" };
    var hello_expr = Expr{ .LITERAL = literal_hello };
    var eql_eql_str = Expr{ .BINARY = .{
        .left = &string_expr,
        .operator = .DOUBLE_EQUAL,
        .right = &hello_expr,
    } };
    eval_res = try interpreter.evaluate(&eql_eql_str);
    std.debug.assert(std.meta.activeTag(eval_res) == .TRUE);
    // Evaluating a binary expr that compares two thigns that are not the same
    const literal_false = Literal{ .FALSE = {} };
    var false_expr = Expr{ .LITERAL = literal_false };
    var false_binary_expr = Expr{ .BINARY = .{
        .left = &eql_eql_str,
        .operator = .DOUBLE_EQUAL,
        .right = &false_expr,
    } };
    eval_res = try interpreter.evaluate(&false_binary_expr);
    std.debug.assert(std.meta.activeTag(eval_res) == .FALSE);
    // Evaluating a binary expr that compares two things that are not of the same type
    var num_expr = Expr{ .LITERAL = .{ .NUMBER = 3.2 } };
    var str_num_bin_expr = Expr{ .BINARY = .{
        .left = &hello_expr,
        .operator = .GREATER,
        .right = &num_expr,
    } };
    const eval_res_two = interpreter.evaluate(&str_num_bin_expr);
    std.debug.assert(std.meta.isError(eval_res_two));
}

test "evaluate unary" {
    var literal_number_expr = Expr{ .LITERAL = Literal{ .NUMBER = 3.4 } };
    const neg_number_expr = Expr{ .UNARY = .{ .NEGATIVE = &literal_number_expr } };
    var interpreter = Interpreter{};
    var eval_res = try interpreter.evaluate(&neg_number_expr);
    std.debug.assert(std.meta.activeTag(eval_res) == .NUMBER);
    std.debug.assert(eval_res.NUMBER == -3.4);
    var literal_true_expr = Expr{ .LITERAL = Literal{ .TRUE = {} } };
    const not_expr = Expr{ .UNARY = .{ .NOT = &literal_true_expr } };
    eval_res = try interpreter.evaluate(&not_expr);
    std.debug.assert(std.meta.activeTag(eval_res) == .FALSE);
    var literal_str_expr = Expr{ .LITERAL = Literal{ .STRING = "hello" } };
    const neg_str_expr = Expr{ .UNARY = .{ .NEGATIVE = &literal_str_expr } };
    const eval_res_two = interpreter.evaluate(&neg_str_expr);
    std.debug.assert(std.meta.isError(eval_res_two));
}

test "evaluate ternary" {}
