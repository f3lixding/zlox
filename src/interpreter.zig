const std = @import("std");
const Literal = @import("ast.zig").Literal;
const Expr = @import("ast.zig").Expr;

pub const InterpreterError = error{
    OperationNotSupported,
};

pub const Interpreter = struct {
    pub fn evaluate(self: *Interpreter, expr: *const Expr) InterpreterError!Literal {
        return switch (expr.*) {
            .LITERAL => |*l| l,
            .UNARY => |*u| self.evaluate(u.expr),
            .BINARY => |*b| {
                const left = try self.evaluate(b.left);
                const right = try self.evaluate(b.right);
                const op = &b.operator;
                switch (op.*) {
                    .DOUBLE_EQUAL => {
                        const left_tag = std.meta.activeTag(left.*);
                        const right_tag = std.meta.activeTag(right.*);
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
                        const left_tag = std.meta.activeTag(left.*);
                        const right_tag = std.meta.activeTag(right.*);
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
                        const left_tag = std.meta.activeTag(left.*);
                        const right_tag = std.meta.activeTag(right.*);
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
                        const left_tag = std.meta.activeTag(left.*);
                        const right_tag = std.meta.activeTag(right.*);
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
                        const left_tag = std.meta.activeTag(left.*);
                        const right_tag = std.meta.activeTag(right.*);
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
                        const left_tag = std.meta.activeTag(left.*);
                        const right_tag = std.meta.activeTag(right.*);
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
                        const left_tag = std.meta.activeTag(left.*);
                        const right_tag = std.meta.activeTag(right.*);
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
                        const left_tag = std.meta.activeTag(left.*);
                        const right_tag = std.meta.activeTag(right.*);
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
                        const left_tag = std.meta.activeTag(left.*);
                        const right_tag = std.meta.activeTag(right.*);
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
                        const left_tag = std.meta.activeTag(left.*);
                        const right_tag = std.meta.activeTag(right.*);
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
            .TERNARY => |*t| {
                _ = t;
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

test "truthy falsey" {}
