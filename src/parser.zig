const std = @import("std");
const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;
const Expr = @import("ast.zig").Expr;

// Parsers is like scanner but for parsing (scanner is for lexing)
// Parser takes a list of tokens and produces ast
// The composition of the functions is the embodiment of the the following grammar:
// expression     → equality ;
// equality       → comparison ( ( "!=" | "==" ) comparison )* ;
// comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
// term           → factor ( ( "-" | "+" ) factor )* ;
// factor         → unary ( ( "/" | "*" ) unary )* ;
// unary          → ( "!" | "-" ) unary
//                | primary ;
// primary        → NUMBER | STRING | "true" | "false" | "nil"
//                | "(" expression ")" ;
pub const Parser = struct {
    tokens: std.ArrayList(Token),
    cur_idx: usize = 0,
    // This is a managed struct
    alloc: std.mem.Allocator,

    pub fn getAST(self: *Parser) void {
        _ = self;
    }

    fn expression(self: *Parser) Expr {
        _ = self;
    }

    fn equality(self: *Parser) Expr {
        _ = self;
    }

    fn comparison(self: *Parser) Expr {
        _ = self;
    }

    fn term(self: *Parser) Expr {
        _ = self;
    }

    fn factor(self: *Parser) Expr {
        _ = self;
    }

    fn unary(self: *Parser) Expr {
        _ = self;
    }

    fn primary(self: *Parser) Expr {
        _ = self;
    }

    // Helper function to match expected tokens and to advance cur_idx
    // i.e. consuming the token
    fn match(self: *Parser, to_match: []const TokenType) void {
        _ = self;
        _ = to_match;
    }
};
