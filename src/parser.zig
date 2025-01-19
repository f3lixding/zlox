const std = @import("std");
const Token = @import("token.zig").Token;
const TokenType = @import("token.zig").TokenType;
const Expr = @import("ast.zig").Expr;
const Operator = @import("ast.zig").Operator;
const Literal = @import("ast.zig").Literal;
const Location = @import("ast.zig").Location;
const Statement = @import("ast.zig").Statement;

pub const ParsingError = error{
    MalformedBuffer,
    MissingSemicolon,
} || std.mem.Allocator.Error || std.fmt.ParseFloatError || GroupParsingError || TernaryParsingError || BinaryExprParsingError;

pub const GroupParsingError = error{MissingParen};

pub const TernaryParsingError = error{
    TernaryMissingCond,
    TernaryMissingSemicolon,
};

pub const BinaryExprParsingError = error{BinaryExprMissingOperand};

pub const ParsingErrorCtx = struct {
    token: *const Token,
    err: ParsingError,
};

// Parsers is like scanner but for parsing (scanner is for lexing)
// Parser takes a list of tokens and produces ast
// The composition of the functions is the embodiment of the the following grammar:
//
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
    errors: std.ArrayList(ParsingErrorCtx),
    cur_idx: usize = 0,
    // This is a managed struct
    alloc: std.mem.Allocator,

    pub fn init(alloc: std.mem.Allocator) Parser {
        return .{
            .tokens = std.ArrayList(Token).init(alloc),
            .errors = std.ArrayList(ParsingErrorCtx).init(alloc),
            .alloc = alloc,
        };
    }

    // This is the main parsing function
    pub fn parse(self: *Parser) !std.ArrayList(Statement) {
        var res = std.ArrayList(Statement).init();
        while (self.cur_idx < self.tokens.items.len) {
            const stmt = try self.statement();
            res.append(stmt);
        }
        return res;
    }

    pub fn getAST(self: *Parser) !*Expr {
        const expr = try self.expression();
        return expr;
    }

    pub fn deinit(self: *Parser) void {
        self.tokens.deinit();
        self.errors.deinit();
    }

    fn statement(self: *Parser) !Statement {
        if (self.match(.PRINT)) |_| {
            const expr = try self.expression();
            self.match(.SEMICOLON) orelse return error.MissingSemicolon;
            return Statement{ .PRINT = expr };
        } else {
            const expr = try self.expression();
            self.match(.SEMICOLON) orelse return error.MissingSemicolon;
            return Statement{ .EXPR = expr };
        }
    }

    fn expression(self: *Parser) !*Expr {
        const expr = try self.ternary();
        return expr;
    }

    fn ternary(self: *Parser) !*Expr {
        const cur_idx = self.cur_idx;
        var rule_lvl_err: ?ParsingError = null;
        if (self.match(&[_]TokenType{.QUESTION_MARK})) |_| {
            rule_lvl_err = error.TernaryMissingCond;
            try self.errors.append(.{
                .err = rule_lvl_err.?,
                .token = &self.tokens.items[cur_idx],
            });
            if (self.equality()) |res| {
                res.deinit(self.alloc);
            } else |_| {}
            if (self.match(&[_]TokenType{.COLON})) |_| {
                if (self.equality()) |res| {
                    res.deinit(self.alloc);
                } else |_| {}
            }
            return rule_lvl_err.?;
        }
        var cond_res = self.equality();
        if (self.match(&[_]TokenType{.QUESTION_MARK})) |question_mark_token| ques_mark_blk: {
            if (cond_res) |expr| {
                const pos_expr_res = self.equality();
                errdefer if (pos_expr_res) |pos_expr| {
                    pos_expr.deinit(self.alloc);
                } else |_| {};
                if (std.meta.isError(pos_expr_res) and rule_lvl_err == null) {
                    if (pos_expr_res) |_| {} else |err| {
                        rule_lvl_err = err;
                    }
                }
                if (self.match(&[_]TokenType{.COLON})) |_| {
                    const neg_expr_res = self.equality();
                    errdefer if (pos_expr_res) |pos_expr| {
                        pos_expr.deinit(self.alloc);
                    } else |_| {};
                    if (std.meta.isError(neg_expr_res) and rule_lvl_err == null) {
                        if (neg_expr_res) |_| {} else |err| {
                            rule_lvl_err = err;
                        }
                    }
                    if (pos_expr_res) |pos_expr| {
                        if (neg_expr_res) |neg_expr| {
                            const complete_ternary_expr = try self.alloc.create(Expr);
                            errdefer self.alloc.destroy(complete_ternary_expr);
                            if (rule_lvl_err == null) {
                                complete_ternary_expr.* = Expr{ .TERNARY = .{
                                    question_mark_token.getLocation(), .{
                                        .cond = expr,
                                        .pos = pos_expr,
                                        .neg = neg_expr,
                                    },
                                } };
                                cond_res = complete_ternary_expr;
                                break :ques_mark_blk;
                            }
                        } else |err| {
                            if (rule_lvl_err == null) {
                                rule_lvl_err = err;
                            }
                            break :ques_mark_blk;
                        }
                    } else |err| {
                        if (rule_lvl_err == null) {
                            rule_lvl_err = err;
                        }
                        break :ques_mark_blk;
                    }
                } else {
                    try self.errors.append(.{
                        .err = error.TernaryMissingSemicolon,
                        .token = &self.tokens.items[cur_idx],
                    });
                    break :ques_mark_blk;
                }
            } else |_| {
                // We do not have a valid cond expr.
                // Try to parse the positive and negative expr anyway and discard them.
                // There is no need to report the error here because each rule only reports error occurred at its own level.
                if (self.equality()) |expr| expr.deinit(self.alloc) else |_| {}
                if (self.match(&[_]TokenType{.COLON})) |_| {
                    if (self.equality()) |expr| expr.deinit(self.alloc) else |_| {}
                }
                return error.TernaryMissingCond;
            }
        }
        if (rule_lvl_err) |e| {
            return e;
        } else {
            return cond_res catch |e| e;
        }
    }

    fn equality(self: *Parser) !*Expr {
        var rule_lvl_err: ?ParsingError = null;
        const match_input = [_]TokenType{ .BANG_EQUAL, .EQUAL_EQUAL };
        if (self.match(&match_input)) |_| {
            rule_lvl_err = error.BinaryExprMissingOperand;
            try self.errors.append(.{
                .err = rule_lvl_err.?,
                .token = &self.tokens.items[self.cur_idx],
            });
            if (self.comparison()) |res| {
                res.deinit(self.alloc);
            } else |_| {}
            return rule_lvl_err.?;
        }
        var left = self.comparison();
        while (self.match(&match_input)) |op| {
            const right = self.comparison();
            if (left) |left_expr| {
                errdefer left_expr.deinit(self.alloc);
                if (right) |right_expr| {
                    errdefer right_expr.deinit(self.alloc);
                    const operator = try Operator.fromToken(op.*);
                    const new_left = try self.alloc.create(Expr);
                    new_left.* = Expr{ .BINARY = .{
                        op.getLocation(), .{
                            .left = left_expr,
                            .operator = operator,
                            .right = right_expr,
                        },
                    } };
                    left = new_left;
                } else |err| {
                    if (rule_lvl_err == null) {
                        rule_lvl_err = err;
                    }
                }
            } else |err| {
                if (rule_lvl_err == null) {
                    rule_lvl_err = err;
                }
            }
        }
        if (rule_lvl_err) |err| {
            return err;
        } else {
            return left;
        }
    }

    fn comparison(self: *Parser) !*Expr {
        var rule_lvl_err: ?ParsingError = null;
        const match_input = [_]TokenType{ .GREATER, .GREATER_EQUAL, .LESS, .LESS_EQUAL };
        if (self.match(&match_input)) |_| {
            rule_lvl_err = error.BinaryExprMissingOperand;
            try self.errors.append(.{
                .err = rule_lvl_err.?,
                .token = &self.tokens.items[self.cur_idx],
            });
            if (self.term()) |res| {
                res.deinit(self.alloc);
            } else |_| {}
            return rule_lvl_err.?;
        }
        var left = self.term();
        while (self.match(&match_input)) |op| {
            const right = self.term();
            if (left) |left_expr| {
                errdefer left_expr.deinit(self.alloc);
                if (right) |right_expr| {
                    errdefer right_expr.deinit(self.alloc);
                    const operator = try Operator.fromToken(op.*);
                    const new_left = try self.alloc.create(Expr);
                    new_left.* = Expr{ .BINARY = .{
                        op.getLocation(), .{
                            .left = left_expr,
                            .operator = operator,
                            .right = right_expr,
                        },
                    } };
                    left = new_left;
                } else |err| {
                    if (rule_lvl_err == null) {
                        rule_lvl_err = err;
                    }
                }
            } else |err| {
                if (rule_lvl_err == null) {
                    rule_lvl_err = err;
                }
            }
        }
        if (rule_lvl_err) |err| {
            return err;
        } else {
            return left;
        }
    }

    fn term(self: *Parser) !*Expr {
        var rule_lvl_err: ?ParsingError = null;
        const match_input = [_]TokenType{ .MINUS, .PLUS };
        if (self.match(&match_input)) |_| {
            rule_lvl_err = error.BinaryExprMissingOperand;
            try self.errors.append(.{
                .err = rule_lvl_err.?,
                .token = &self.tokens.items[self.cur_idx],
            });
            if (self.factor()) |res| {
                res.deinit(self.alloc);
            } else |_| {}
            return rule_lvl_err.?;
        }
        var left = self.factor();
        while (self.match(&match_input)) |op| {
            const right = self.factor();
            if (left) |left_expr| {
                errdefer left_expr.deinit(self.alloc);
                if (right) |right_expr| {
                    errdefer right_expr.deinit(self.alloc);
                    const operator = try Operator.fromToken(op.*);
                    const new_left = try self.alloc.create(Expr);
                    new_left.* = Expr{ .BINARY = .{
                        op.getLocation(), .{
                            .left = left_expr,
                            .operator = operator,
                            .right = right_expr,
                        },
                    } };
                    left = new_left;
                } else |err| {
                    if (rule_lvl_err == null) {
                        rule_lvl_err = err;
                    }
                }
            } else |err| {
                if (rule_lvl_err == null) {
                    rule_lvl_err = err;
                }
            }
        }
        if (rule_lvl_err) |err| {
            return err;
        } else {
            return left;
        }
    }

    fn factor(self: *Parser) !*Expr {
        var rule_lvl_err: ?ParsingError = null;
        const match_input = [_]TokenType{ .SLASH, .STAR };
        if (self.match(&match_input)) |_| {
            rule_lvl_err = error.BinaryExprMissingOperand;
            try self.errors.append(.{
                .err = rule_lvl_err.?,
                .token = &self.tokens.items[self.cur_idx],
            });
            if (self.unary()) |res| {
                res.deinit(self.alloc);
            } else |_| {}
            return rule_lvl_err.?;
        }
        var left = self.unary();
        while (self.match(&match_input)) |op| {
            const right = self.unary();
            if (left) |left_expr| {
                errdefer left_expr.deinit(self.alloc);
                if (right) |right_expr| {
                    errdefer right_expr.deinit(self.alloc);
                    const operator = try Operator.fromToken(op.*);
                    const new_left = try self.alloc.create(Expr);
                    new_left.* = Expr{ .BINARY = .{
                        op.getLocation(), .{
                            .left = left_expr,
                            .operator = operator,
                            .right = right_expr,
                        },
                    } };
                    left = new_left;
                } else |err| {
                    if (rule_lvl_err == null) {
                        rule_lvl_err = err;
                    }
                }
            } else |err| {
                if (rule_lvl_err == null) {
                    rule_lvl_err = err;
                }
            }
        }
        if (rule_lvl_err) |err| {
            return err;
        } else {
            return left;
        }
    }

    fn unary(self: *Parser) !*Expr {
        const match_input = [_]TokenType{ .BANG, .MINUS };
        if (self.match(&match_input)) |op| {
            const right = try self.unary();
            const res = try self.alloc.create(Expr);
            errdefer self.alloc.destroy(res);
            switch (op.*) {
                .BANG => {
                    res.* = Expr{ .UNARY = .{ op.getLocation(), .{ .NOT = right } } };
                },
                .MINUS => {
                    res.* = Expr{ .UNARY = .{ op.getLocation(), .{ .NEGATIVE = right } } };
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
                .TRUE, .FALSE, .NIL => Expr{ .LITERAL = .{ token.getLocation(), try Literal.fromToken(self.alloc, token) } },
                else => unreachable,
            };
            return res;
        }
        if (self.match(&[_]TokenType{ .NUMBER, .STRING })) |token| {
            res.* = switch (token.*) {
                .NUMBER, .STRING => Expr{ .LITERAL = .{ token.getLocation(), try Literal.fromToken(self.alloc, token) } },
                else => unreachable,
            };
            return res;
        }
        if (self.match(&[_]TokenType{.LEFT_PAREN})) |token| {
            const expr = try self.expression();
            errdefer expr.deinit(self.alloc);
            if (self.match(&[_]TokenType{.RIGHT_PAREN})) |_| {
                res.* = Expr{ .GROUPING = .{ token.getLocation(), .{ .expr = expr } } };
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
        .errors = std.ArrayList(ParsingErrorCtx).init(std.testing.allocator),
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
    const errors = std.ArrayList(ParsingErrorCtx).init(alloc);
    defer errors.deinit();
    var parser = Parser{
        .alloc = alloc,
        .errors = errors,
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
    std.debug.assert(std.meta.activeTag(res.GROUPING.@"1".expr.LITERAL.@"1") == .FALSE);
}

test "overall parsing one" {
    // Testing overall parsing logic
    const alloc = std.testing.allocator;
    const tokens = std.ArrayList(Token).init(alloc);
    defer tokens.deinit();
    const errors = std.ArrayList(ParsingErrorCtx).init(alloc);
    defer errors.deinit();
    var parser = Parser{
        .alloc = alloc,
        .errors = errors,
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
    const main_op = &res.BINARY.@"1".operator;
    std.debug.assert(std.meta.activeTag(main_op.*) == .MINUS);
    const left = res.BINARY.@"1".left;
    std.debug.assert(std.meta.activeTag(left.*) == .BINARY);
    const left_left = left.BINARY.@"1".left;
    std.debug.assert(std.meta.activeTag(left_left.*) == .LITERAL);
    std.debug.assert(std.meta.activeTag(left_left.LITERAL.@"1") == .NUMBER);
    std.debug.assert(left_left.LITERAL.@"1".NUMBER == 6.4);
    const left_right = left.BINARY.@"1".right;
    std.debug.assert(std.meta.activeTag(left_right.*) == .LITERAL);
    std.debug.assert(std.meta.activeTag(left_right.LITERAL.@"1") == .NUMBER);
    std.debug.assert(left_right.LITERAL.@"1".NUMBER == 2.1);
    const left_left_op = &res.BINARY.@"1".left.BINARY.@"1".operator;
    std.debug.assert(std.meta.activeTag(left_left_op.*) == .SLASH);
    const right = res.BINARY.@"1".right;
    std.debug.assert(std.meta.activeTag(right.*) == .LITERAL);
    std.debug.assert(std.meta.activeTag(right.LITERAL.@"1") == .NUMBER);
    std.debug.assert(right.LITERAL.@"1".NUMBER == 3.0);
}

test "overall parsing ternary" {
    const alloc = std.testing.allocator;
    const tokens = std.ArrayList(Token).init(alloc);
    defer tokens.deinit();
    const errors = std.ArrayList(ParsingErrorCtx).init(alloc);
    defer errors.deinit();
    var parser = Parser{
        .alloc = alloc,
        .errors = errors,
        .tokens = tokens,
    };
    const test_tokens = [_]Token{
        Token{ .NUMBER = .{ .line = 0, .lexeme = "100" } },
        Token{ .EQUAL_EQUAL = .{ .line = 0 } },
        Token{ .NUMBER = .{ .line = 0, .lexeme = "100" } },
        Token{ .QUESTION_MARK = .{ .line = 0 } },
        Token{ .NUMBER = .{ .line = 0, .lexeme = "1.1" } },
        Token{ .COLON = .{ .line = 0 } },
        Token{ .NUMBER = .{ .line = 0, .lexeme = "2.2" } },
    };
    parser.tokens.appendSlice(&test_tokens) catch unreachable;
    defer parser.deinit();
    const res = parser.getAST() catch unreachable;
    defer res.deinit(parser.alloc);
    std.debug.assert(std.meta.activeTag(res.*) == .TERNARY);
    const cond = res.TERNARY.@"1".cond;
    std.debug.assert(std.meta.activeTag(cond.*) == .BINARY);
    const pos = res.TERNARY.@"1".pos;
    std.debug.assert(std.meta.activeTag(pos.*) == .LITERAL);
    std.debug.assert(std.meta.activeTag(pos.LITERAL.@"1") == .NUMBER);
    const neg = res.TERNARY.@"1".neg;
    std.debug.assert(std.meta.activeTag(neg.*) == .LITERAL);
    std.debug.assert(std.meta.activeTag(neg.LITERAL.@"1") == .NUMBER);
}

test "error recovery ternary" {
    const alloc = std.testing.allocator;
    const tokens = std.ArrayList(Token).init(alloc);
    defer tokens.deinit();
    const errors = std.ArrayList(ParsingErrorCtx).init(alloc);
    defer errors.deinit();
    var parser = Parser{
        .alloc = alloc,
        .errors = errors,
        .tokens = tokens,
    };
    const test_tokens = [_]Token{
        Token{ .QUESTION_MARK = .{ .line = 0 } },
        Token{ .NUMBER = .{ .line = 0, .lexeme = "1.1" } },
        Token{ .COLON = .{ .line = 0 } },
        Token{ .NUMBER = .{ .line = 0, .lexeme = "2.2" } },
    };
    parser.tokens.appendSlice(&test_tokens) catch unreachable;
    defer parser.deinit();
    const res = parser.getAST();
    if (res) |ast| {
        defer ast.deinit(parser.alloc);
    } else |_| {}
    std.debug.assert(res == error.TernaryMissingCond);
    std.debug.assert(parser.errors.items.len == 1);
    const err = &parser.errors.items[0];
    std.debug.assert(std.meta.activeTag(err.token.*) == .QUESTION_MARK);
}
