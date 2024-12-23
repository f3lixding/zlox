const std = @import("std");
const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;
const Expr = @import("ast.zig").Expr;
const Operator = @import("ast.zig").Operator;

const ParsingError = error{
    MissingParen,
    MalformedBuffer,
} || std.mem.Allocator.Error || std.fmt.ParseFloatError;

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

    pub fn getAST(self: *Parser) !*Expr {
        const expr = try self.expression();
        return expr;
    }

    pub fn deinit(self: *Parser) void {
        self.tokens.deinit();
    }

    fn expression(self: *Parser) !*Expr {
        const expr = try self.equality();
        return expr;
    }

    fn equality(self: *Parser) !*Expr {
        var left = try self.comparison();
        const match_input = [_]TokenType{ .BANG_EQUAL, .EQUAL_EQUAL };
        while (self.match(&match_input)) |op| {
            const right = try self.comparison();
            const operator = try Operator.fromToken(op.*);
            const new_left = try self.alloc.create(Expr);
            new_left.* = Expr{ .BINARY = .{ .left = left, .operator = operator, .right = right } };
            left = new_left;
        }
        return left;
    }

    fn comparison(self: *Parser) !*Expr {
        var left = try self.term();
        const match_input = [_]TokenType{ .GREATER, .GREATER_EQUAL, .LESS, .LESS_EQUAL };
        while (self.match(&match_input)) |op| {
            const right = try self.term();
            const operator = try Operator.fromToken(op.*);
            const new_left = try self.alloc.create(Expr);
            new_left.* = Expr{ .BINARY = .{ .left = left, .operator = operator, .right = right } };
            left = new_left;
        }
        return left;
    }

    fn term(self: *Parser) !*Expr {
        var left = try self.factor();
        const match_input = [_]TokenType{ .MINUS, .PLUS };
        while (self.match(&match_input)) |op| {
            const right = try self.term();
            const operator = try Operator.fromToken(op.*);
            const new_left = try self.alloc.create(Expr);
            new_left.* = Expr{ .BINARY = .{ .left = left, .operator = operator, .right = right } };
            left = new_left;
        }
        return left;
    }

    fn factor(self: *Parser) !*Expr {
        var left = try self.unary();
        const match_input = [_]TokenType{ .SLASH, .STAR };
        while (self.match(&match_input)) |op| {
            const right = try self.unary();
            const operator = try Operator.fromToken(op.*);
            const new_left = try self.alloc.create(Expr);
            new_left.* = Expr{ .BINARY = .{ .left = left, .operator = operator, .right = right } };
            left = new_left;
        }
        return left;
    }

    fn unary(self: *Parser) !*Expr {
        const match_input = [_]TokenType{ .BANG, .MINUS };
        if (self.match(&match_input)) |op| {
            const right = try self.unary();
            const res = try self.alloc.create(Expr);
            errdefer self.alloc.destroy(res);
            switch (op.*) {
                .BANG => {
                    res.* = Expr{ .UNARY = .{ .NOT = right } };
                },
                .MINUS => {
                    res.* = Expr{ .UNARY = .{ .NEGATIVE = right } };
                },
                else => unreachable,
            }
            return res;
        }
        return self.primary();
    }

    fn primary(self: *Parser) ParsingError!*Expr {
        const res = try self.alloc.create(Expr);
        errdefer self.alloc.destroy(res);
        if (self.match(&[_]TokenType{ .FALSE, .TRUE, .NIL })) |token| {
            res.* = switch (token.*) {
                .TRUE => |_| Expr{ .LITERAL = .TRUE },
                .FALSE => |_| Expr{ .LITERAL = .FALSE },
                .NIL => |_| Expr{ .LITERAL = .NIL },
                else => unreachable,
            };
            return res;
        }
        if (self.match(&[_]TokenType{ .NUMBER, .STRING })) |token| {
            res.* = switch (token.*) {
                .NUMBER => |inner_struct| Expr{ .LITERAL = .{ .NUMBER = try std.fmt.parseFloat(f64, inner_struct.lexeme) } },
                .STRING => |inner_struct| Expr{ .LITERAL = .{ .STRING = inner_struct.lexeme } },
                else => unreachable,
            };
            return res;
        }
        if (self.match(&[_]TokenType{.LEFT_PAREN})) |_| {
            const expr = try self.expression();
            errdefer expr.deinit(self.alloc);
            if (self.match(&[_]TokenType{.RIGHT_PAREN})) |_| {
                res.* = Expr{ .GROUPING = .{ .expr = expr } };
                return res;
            } else {
                return error.MissingParen;
            }
        }
        return error.MalformedBuffer;
    }

    // Helper function to match expected tokens and to advance cur_idx
    // i.e. consuming the token
    fn match(self: *Parser, to_match: []const TokenType) ?*Token {
        const cur_token = &self.tokens.items[self.cur_idx];
        for (to_match) |token_type| {
            if (std.meta.activeTag(cur_token.*) == token_type) {
                self.cur_idx += 1;
                return cur_token;
            }
        }
        return null;
    }

    // Helper function return the token before the one pointed to by cur_idx
    fn previous(self: Parser) ?*Token {
        _ = self;
        return null;
    }
};

test "match" {
    var parser = Parser{
        .tokens = std.ArrayList(Token).init(std.testing.allocator),
        .alloc = std.testing.allocator,
    };
    defer parser.deinit();
    const ident = Token{ .IDENTIFIER = .{ .line = 0, .lexeme = "hello" } };
    try parser.tokens.append(ident);
    const res = parser.match(&[_]TokenType{.IDENTIFIER});
    std.debug.assert(res != null);
    std.debug.assert(std.meta.activeTag(res.?.*) == .IDENTIFIER);
}

test "primary" {
    // This is the "bottom" method for parsing so we'll need to write a test for it
    const alloc = std.testing.allocator;
    const tokens = std.ArrayList(Token).init(alloc);
    defer tokens.deinit();
    var parser = Parser{
        .alloc = alloc,
        .tokens = tokens,
    };
    const test_tokens = [_]Token{
        Token{ .LEFT_PAREN = .{ .line = 0 } },
        Token{ .FALSE = .{ .line = 0 } },
        Token{ .RIGHT_PAREN = .{ .line = 0 } },
    };
    parser.tokens.appendSlice(&test_tokens) catch unreachable;
    defer parser.deinit();
    const res = parser.primary() catch unreachable;
    defer res.deinit(parser.alloc);
    std.debug.print("{any}\n", .{res.*});
}
