const std = @import("std");

pub const Expr = union(enum) {
    LITERAL: Literal,
    UNARY: Unary,
    BINARY: Binary,
    GROUPING: Grouping,
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
