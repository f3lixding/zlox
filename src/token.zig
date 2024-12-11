const std = @import("std");
pub const TokenType = enum {
    // Single character tokens:
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,

    // One or two character tokens:
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,

    // Literals:
    IDENTIFIER,
    STRING,
    NUMBER,

    // Keywords:
    AND,
    CLASS,
    ELSE,
    FALSE,
    FOR,
    FUN,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,

    EOF,
};

pub const Token = union(TokenType) {
    // Single character tokens:
    LEFT_PAREN: struct {
        line: usize,
        lexeme: u8 = '(',
    },
    RIGHT_PAREN: struct {
        line: usize,
        lexeme: u8 = ')',
    },
    LEFT_BRACE: struct {
        line: usize,
        lexeme: u8 = '{',
    },
    RIGHT_BRACE: struct {
        line: usize,
        lexeme: u8 = '}',
    },
    COMMA: struct {
        line: usize,
        lexeme: u8 = ',',
    },
    DOT: struct {
        line: usize,
        lexeme: u8 = '.',
    },
    MINUS: struct {
        line: usize,
        lexeme: u8 = '-',
    },
    PLUS: struct {
        line: usize,
        lexeme: u8 = '+',
    },
    SEMICOLON: struct {
        line: usize,
        lexeme: u8 = ';',
    },
    SLASH: struct {
        line: usize,
        lexeme: u8 = '/',
    },
    STAR: struct {
        line: usize,
        lexeme: u8 = '*',
    },

    // One or two character tokens:
    BANG: struct {
        line: usize,
        lexeme: u8 = '!',
    },
    BANG_EQUAL: struct {
        line: usize,
        lexeme: *const [2]u8 = "!=",
    },
    EQUAL: struct {
        line: usize,
        lexeme: u8 = '=',
    },
    EQUAL_EQUAL: struct {
        line: usize,
        lexeme: *const [2]u8 = "==",
    },
    GREATER: struct {
        line: usize,
        lexeme: u8 = '>',
    },
    GREATER_EQUAL: struct {
        line: usize,
        lexeme: *const [2]u8 = ">=",
    },
    LESS: struct {
        line: usize,
        lexeme: u8 = '<',
    },
    LESS_EQUAL: struct {
        line: usize,
        lexeme: *const [2]u8 = "<=",
    },

    // Literals:
    IDENTIFIER: struct {
        line: usize,
        lexeme: []const u8,
    },
    STRING: struct {
        line: usize,
        lexeme: []const u8,
    },
    NUMBER: struct {
        line: usize,
        lexeme: []const u8,
    },

    // Keywords:
    AND: struct {
        line: usize,
        lexeme: *const [3]u8 = "and",
    },
    CLASS: struct {
        line: usize,
        lexeme: *const [5]u8 = "class",
    },
    ELSE: struct {
        line: usize,
        lexeme: *const [4]u8 = "else",
    },
    FALSE: struct {
        line: usize,
        lexeme: *const [5]u8 = "false",
    },
    FOR: struct {
        line: usize,
        lexeme: *const [3]u8 = "for",
    },
    FUN: struct {
        line: usize,
        lexeme: *const [3]u8 = "fun",
    },
    IF: struct {
        line: usize,
        lexeme: *const [2]u8 = "if",
    },
    NIL: struct {
        line: usize,
        lexeme: *const [3]u8 = "nil",
    },
    OR: struct {
        line: usize,
        lexeme: *const [2]u8 = "or",
    },
    PRINT: struct {
        line: usize,
        lexeme: *const [5]u8 = "print",
    },
    RETURN: struct {
        line: usize,
        lexeme: *const [6]u8 = "return",
    },
    SUPER: struct {
        line: usize,
        lexeme: *const [5]u8 = "super",
    },
    THIS: struct {
        line: usize,
        lexeme: *const [4]u8 = "this",
    },
    TRUE: struct {
        line: usize,
        lexeme: *const [4]u8 = "true",
    },
    VAR: struct {
        line: usize,
        lexeme: *const [3]u8 = "var",
    },
    WHILE: struct {
        line: usize,
        lexeme: *const [5]u8 = "while",
    },

    EOF: struct {
        line: usize,
    },

    // This needs to take *Token because otherwise you cannot get a valid ptr to the fields of inner struct
    // We need to get the ptr of the field of the inner struct because some variants of the union has an u8 for lexeme
    // For that reason, we would need to take the ptr of that field and create a fat ptr from it. And that ptr would
    // need to remain valid after we have left the scope of this function.
    // You'll also notice that we deference it in the switch case. This is because inline else does _not_ work for ptr.
    // Another thing worth noting is the fact that we capture the ptr of the inner_struct in the switch case arm.
    // This is because if we capture its value, the references from the capture would exhibit the same problem (i.e.
    // they won't be valid as soon as we leave the scope of the function).
    // See [pass by value param](https://ziglang.org/documentation/master/#toc-Pass-by-value-Parameters) for more info.
    pub fn getLexeme(self: *Token) ?[]const u8 {
        switch (self.*) {
            inline else => |*inner_struct| {
                if (@hasField(@TypeOf(inner_struct.*), "lexeme")) {
                    const lexeme = &inner_struct.lexeme;
                    if (@TypeOf(lexeme.*) == u8) {
                        return @as(*const [1]u8, lexeme)[0..1];
                    } else {
                        return lexeme.*;
                    }
                } else {
                    return null;
                }
            },
        }
    }
};

test "test get lexeme" {
    var token = Token{ .IDENTIFIER = .{ .line = 0, .lexeme = "hello" } };
    try std.testing.expectEqualStrings("hello", token.getLexeme().?);
    var token2 = Token{ .EQUAL = .{ .line = 0 } };
    try std.testing.expectEqualStrings("=", token2.getLexeme().?);
}
