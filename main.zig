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
                if (line == null) return null;
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
        if (token == null) return null;

        if (token.?[0] == '(') {
            var list = std.ArrayList(Expression).init(allocator);

            while (self.next(allocator, stdin)) |subexpression| {
                switch (subexpression) {
                    .end => break,
                    else => list.append(subexpression) catch unreachable,
                }
            }

            return Expression{ .list = list.items };
        }

        if (token.?[0] == ')') {
            return Expression{ .end = {} };
        }

        const number = std.fmt.parseInt(i32, token.?, 10) catch {
            var word = std.ArrayList(u8).init(allocator);
            word.appendSlice(token.?) catch unreachable;
            return Expression{ .word = word.items };
        };
        return Expression{ .number = number };
    }
};

fn evalList(store: std.StringHashMap(Expression), list: []const Expression) Expression {
    const function = eval(store, list[0]);
    switch (function) {
        .builtin => |builtin| {
            if (std.mem.eql(u8, builtin, "+")) {
                var result: i32 = 0;
                for (list[1..]) |argument| {
                    result += eval(store, argument).number; // safety
                }
                return Expression{ .number = result };
            } else unreachable;
        },
        else => unreachable,
    }
}

fn eval(store: std.StringHashMap(Expression), expression: Expression) Expression {
    return switch (expression) {
        .number => expression,
        .word => |word| store.get(word).?,
        .list => |list| if (list.len == 0) expression else evalList(store, list),
        else => unreachable,
    };
}

fn print(stdout: anytype, expression: Expression) !void {
    switch (expression) {
        .number => |number| try stdout.print("{d}", .{number}),
        .word => |word| try stdout.print("{s}", .{word}),
        .list => |list| {
            try stdout.writeByte('(');
            if (list.len > 0) {
                try print(stdout, list[0]);
                for (list[1..]) |subexpression| {
                    try stdout.writeByte(' ');
                    try print(stdout, subexpression);
                }
            }
            try stdout.writeByte(')');
        },
        .builtin => |builtin| try stdout.print("builtin {s}", .{builtin}),
        .end => unreachable,
    }
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    var store = std.StringHashMap(Expression).init(allocator);
    try store.put("+", Expression{ .builtin = "+" });

    const tokenIterator = TokenIterator{
        .buffer = undefined,
        .wordIterator = null,
        .word = null,
    };

    var expressionIterator = ExpressionIterator{ .tokenIterator = tokenIterator };
    while (expressionIterator.next(allocator, stdin)) |expression| {
        try print(stdout, eval(store, expression));
        try stdout.writeByte('\n');
    }
}
