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
//
// Or, with the addition of ternary:
// expression     → ternary ;
// ternary        → equality ( "?" equality ":" equality )? ;
// equality       → comparison ( ( "!=" | "==" ) comparison )* ;
// comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
// term           → factor ( ( "-" | "+" ) factor )* ;
// factor         → unary ( ( "/" | "*" ) unary )* ;
// unary          → ( "!" | "-" ) unary
//                | primary ;
// primary        → NUMBER | STRING | "true" | "false" | "nil"
//                | "(" expression ")" ;
//
// Some things to note here:
// - This parser has left associativity. This means that in a series of the same operator,
// the order of evaluation is from left to right.
// - This construction of AST done with this parser is done in a "top down" manner.
// - With the evaluation of each rule, it would first assume whatever it comes across next
// in line in the stream of tokens as the left part of BINARY expr, with the exception
// of the evaluation of unary rule. If it turns out that the token is, in fact, not a part of
// a BINARY expr, it will just return the left expr as is.
// - The nested components of said BINARY expr is to be evaluated with the rule that is one
// level higher in terms of precedence. This is because while the construction is done top
// down, the evaluation is done bottom up ("children" in the tree has higher precendence).
// - User buffer specific data ARE copied here and would be owned by the AST.
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
        const expr = try self.ternary();
        return expr;
    }

    fn ternary(self: *Parser) !*Expr {
        var expr = try self.equality();
        errdefer expr.deinit(self.alloc);
        if (self.match(&[_]TokenType{.QUESTION_MARK})) |_| {
            const positive_expr = try self.equality();
            errdefer positive_expr.deinit(self.alloc);
            if (self.match(&[_]TokenType{.SEMICOLON})) |_| {
                const negative_expr = try self.equality();
                errdefer negative_expr.deinit(self.alloc);
                const complete_ternary_expr = try self.alloc.create(Expr);
                errdefer self.alloc.destroy(complete_ternary_expr);
                complete_ternary_expr.* = Expr{ .TERNARY = .{
                    .cond = expr,
                    .pos = positive_expr,
                    .neg = negative_expr,
                } };
                return complete_ternary_expr;
            } else {
                return error.MalformedBuffer;
            }
        }
        return expr;
    }

    fn equality(self: *Parser) !*Expr {
        var left = try self.comparison();
        const match_input = [_]TokenType{ .BANG_EQUAL, .EQUAL_EQUAL };
        while (self.match(&match_input)) |op| {
            const right = try self.comparison();
            const operator = try Operator.fromToken(op.*);
            const new_left = try self.alloc.create(Expr);
            new_left.* = Expr{ .BINARY = .{
                .left = left,
                .operator = operator,
                .right = right,
            } };
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
            new_left.* = Expr{ .BINARY = .{
                .left = left,
                .operator = operator,
                .right = right,
            } };
            left = new_left;
        }
        return left;
    }

    fn term(self: *Parser) !*Expr {
        var left = try self.factor();
        const match_input = [_]TokenType{ .MINUS, .PLUS };
        while (self.match(&match_input)) |op| {
            const right = try self.factor();
            const operator = try Operator.fromToken(op.*);
            const new_left = try self.alloc.create(Expr);
            new_left.* = Expr{ .BINARY = .{
                .left = left,
                .operator = operator,
                .right = right,
            } };
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
            new_left.* = Expr{ .BINARY = .{
                .left = left,
                .operator = operator,
                .right = right,
            } };
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

    // TODO: enable parsing of identifier
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
        if (self.cur_idx >= self.tokens.items.len) {
            return null;
        }
        const cur_token = &self.tokens.items[self.cur_idx];
        for (to_match) |token_type| {
            if (std.meta.activeTag(cur_token.*) == token_type) {
                self.cur_idx += 1;
                return cur_token;
            }
        }
        return null;
    }

    // At the time of writing, we're at chapter 6.
    // This function will remain incomplete until the latter half of the book.
    // TODO: finish implementing this function.
    fn synchronize(self: *Parser) void {
        // Start by advancing because we wouldn't call this function unless we have run into an error.
        // This means the token pointed to by cur_idx is for sure part of the error.
        self.cur_idx += 1;
        while (self.cur_idx < self.tokens.items.len) : (self.cur_idx += 1) {
            const last_token = &self.tokens.items[self.cur_idx - 1];
            if (std.meta.activeTag(last_token.*) == .SEMICOLON) {
                return;
            }
            switch (self.tokens.items[self.cur_idx]) {
                .CLASS => |_| {},
                .FUN => |_| {},
                .VAR => |_| {},
                .FOR => |_| {},
                .IF => |_| {},
                .WHILE => |_| {},
                .PRINT => |_| {},
                .RETURN => |_| {},
            }
        }
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
    std.debug.assert(std.meta.activeTag(res.*) == .GROUPING);
    std.debug.assert(std.meta.activeTag(res.GROUPING.expr.LITERAL) == .FALSE);
}

test "overall parsing one" {
    // Testing overall parsing logic
    const alloc = std.testing.allocator;
    const tokens = std.ArrayList(Token).init(alloc);
    defer tokens.deinit();
    var parser = Parser{
        .alloc = alloc,
        .tokens = tokens,
    };
    // A bunch of test tokens here to test every rule
    const test_tokens = [_]Token{
        Token{ .NUMBER = .{ .line = 0, .lexeme = "6.4" } },
        Token{ .SLASH = .{ .line = 0 } },
        Token{ .NUMBER = .{ .line = 0, .lexeme = "2.1" } },
        Token{ .MINUS = .{ .line = 0 } },
        Token{ .NUMBER = .{ .line = 0, .lexeme = "3.0" } },
    };
    parser.tokens.appendSlice(&test_tokens) catch unreachable;
    defer parser.deinit();
    const res = parser.getAST() catch unreachable;
    defer res.deinit(parser.alloc);
    std.debug.assert(std.meta.activeTag(res.*) == .BINARY);
    const main_op = &res.BINARY.operator;
    std.debug.assert(std.meta.activeTag(main_op.*) == .MINUS);
    const left = res.BINARY.left;
    std.debug.assert(std.meta.activeTag(left.*) == .BINARY);
    const left_left = left.BINARY.left;
    std.debug.assert(std.meta.activeTag(left_left.*) == .LITERAL);
    std.debug.assert(std.meta.activeTag(left_left.LITERAL) == .NUMBER);
    std.debug.assert(left_left.LITERAL.NUMBER == 6.4);
    const left_right = left.BINARY.right;
    std.debug.assert(std.meta.activeTag(left_right.*) == .LITERAL);
    std.debug.assert(std.meta.activeTag(left_right.LITERAL) == .NUMBER);
    std.debug.assert(left_right.LITERAL.NUMBER == 2.1);
    const left_left_op = &res.BINARY.left.BINARY.operator;
    std.debug.assert(std.meta.activeTag(left_left_op.*) == .SLASH);
    const right = res.BINARY.right;
    std.debug.assert(std.meta.activeTag(right.*) == .LITERAL);
    std.debug.assert(std.meta.activeTag(right.LITERAL) == .NUMBER);
    std.debug.assert(right.LITERAL.NUMBER == 3.0);
}

test "overall parsing two" {
    const alloc = std.testing.allocator;
    const tokens = std.ArrayList(Token).init(alloc);
    defer tokens.deinit();
    var parser = Parser{
        .alloc = alloc,
        .tokens = tokens,
    };
    const test_tokens = [_]Token{
        Token{ .NUMBER = .{ .line = 0, .lexeme = "100" } },
        Token{ .EQUAL_EQUAL = .{ .line = 0 } },
        Token{ .NUMBER = .{ .line = 0, .lexeme = "100" } },
        Token{ .QUESTION_MARK = .{ .line = 0 } },
        Token{ .NUMBER = .{ .line = 0, .lexeme = "1.1" } },
        Token{ .SEMICOLON = .{ .line = 0 } },
        Token{ .NUMBER = .{ .line = 0, .lexeme = "2.2" } },
    };
    parser.tokens.appendSlice(&test_tokens) catch unreachable;
    defer parser.deinit();
    const res = parser.getAST() catch unreachable;
    defer res.deinit(parser.alloc);
    std.debug.print("{any}\n", .{res.*});
}
