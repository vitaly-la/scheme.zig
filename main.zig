const std = @import("std");

const Expression = union(enum) {
    number: i32,
    word: []const u8,
    list: []const Expression,
    builtin: []const u8,
    end,
};

const TokenIterator = struct {
    buffer: [128]u8,
    wordIterator: ?std.mem.TokenIterator(u8, std.mem.DelimiterType.scalar),
    word: ?[]const u8,

    fn next(self: *TokenIterator, stdin: anytype) ?[]const u8 {
        while (true) {
            if (self.wordIterator == null) {
                const line = stdin.readUntilDelimiterOrEof(&self.buffer, '\n') catch unreachable;
                if (line == null) {
                    return null;
                }
                self.wordIterator = std.mem.tokenizeScalar(u8, line.?, ' ');
            }

            if (self.word == null or self.word.?.len == 0) {
                self.word = self.wordIterator.?.next();
                if (self.word == null) {
                    self.wordIterator = null;
                    continue;
                }
            }

            const word = self.word.?;
            const left = std.mem.indexOf(u8, word, "(") orelse word.len;
            const right = std.mem.indexOf(u8, word, ")") orelse word.len;
            const quote = std.mem.indexOf(u8, word, "'") orelse word.len;
            const idx = @max(@min(left, right, quote), 1);

            self.word = word[idx..];
            return word[0..idx];
        }
    }
};

const ExpressionIterator = struct {
    tokenIterator: TokenIterator,

    fn next(self: *ExpressionIterator, allocator: anytype, stdin: anytype) ?Expression {
        const token = self.tokenIterator.next(stdin);
        if (token == null) {
            return null;
        }

        if (token.?[0] == '(') {
            var list = std.ArrayList(Expression).init(allocator);

            while (self.next(allocator, stdin)) |subexpression| {
                if (subexpression == .end) {
                    break;
                }
                list.append(subexpression) catch unreachable;
            }

            return Expression{ .list = list.items };
        }

        if (token.?[0] == ')') {
            return Expression{ .end = {} };
        }

        if (token.?[0] == '\'') {
            var list = std.ArrayList(Expression).init(allocator);

            const subexpression = self.next(allocator, stdin).?;
            list.append(Expression{ .word = "quote" }) catch unreachable;
            list.append(subexpression) catch unreachable;

            return Expression{ .list = list.items };
        }

        const number = std.fmt.parseInt(i32, token.?, 10) catch {
            var word = std.ArrayList(u8).init(allocator);
            word.appendSlice(token.?) catch unreachable;
            return Expression{ .word = word.items };
        };
        return Expression{ .number = number };
    }
};

fn evalList(allocator: anytype, store: *std.StringHashMap(Expression), list: []const Expression) Expression {
    const function = eval(allocator, store, list[0]);
    switch (function) {
        .builtin => |builtin| {
            if (std.mem.eql(u8, builtin, "+")) {
                var result: i32 = 0;
                for (list[1..]) |argument| {
                    const number = eval(allocator, store, argument);
                    if (number == .number) {
                        result += eval(allocator, store, argument).number;
                    } else unreachable;
                }
                return Expression{ .number = result };
            } else if (std.mem.eql(u8, builtin, "quote")) {
                return list[1];
            } else if (std.mem.eql(u8, builtin, "cons")) {
                const head = eval(allocator, store, list[1]);
                const tail = eval(allocator, store, list[2]);
                var newList = std.ArrayList(Expression).init(allocator);

                newList.append(head) catch unreachable;
                if (tail == .list) {
                    newList.appendSlice(tail.list) catch unreachable;
                } else unreachable;

                return Expression{ .list = newList.items };
            } else if (std.mem.eql(u8, builtin, "define")) {
                if (list[1] == .word) {
                    store.put(list[1].word, eval(allocator, store, list[2])) catch unreachable;
                } else unreachable;
                return Expression{ .word = list[1].word };
            } else unreachable;
        },
        else => unreachable,
    }
}

fn eval(allocator: anytype, store: *std.StringHashMap(Expression), expression: Expression) Expression {
    return switch (expression) {
        .number => expression,
        .word => |word| store.get(word).?,
        .list => |list| if (list.len == 0) expression else evalList(allocator, store, list),
        else => unreachable,
    };
}

fn print(stdout: anytype, expression: Expression) void {
    switch (expression) {
        .number => |number| stdout.print("{d}", .{number}) catch unreachable,
        .word => |word| stdout.print("{s}", .{word}) catch unreachable,
        .list => |list| {
            stdout.writeByte('(') catch unreachable;
            if (list.len > 0) {
                print(stdout, list[0]);
                for (list[1..]) |subexpression| {
                    stdout.writeByte(' ') catch unreachable;
                    print(stdout, subexpression);
                }
            }
            stdout.writeByte(')') catch unreachable;
        },
        .builtin => |builtin| stdout.print("builtin {s}", .{builtin}) catch unreachable,
        .end => unreachable,
    }
}

pub fn main() void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    var store = std.StringHashMap(Expression).init(allocator);
    const builtins = [_][]const u8{
        "+",
        "quote",
        "cons",
        "define",
    };
    for (builtins) |builtin| {
        store.put(builtin, Expression{ .builtin = builtin }) catch unreachable;
    }

    const tokenIterator = TokenIterator{
        .buffer = undefined,
        .wordIterator = null,
        .word = null,
    };

    var expressionIterator = ExpressionIterator{ .tokenIterator = tokenIterator };
    while (expressionIterator.next(allocator, stdin)) |expression| {
        print(stdout, eval(allocator, &store, expression));
        stdout.writeByte('\n') catch unreachable;
    }
}
