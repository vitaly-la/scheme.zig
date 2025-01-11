const std = @import("std");

const Expression = union(enum) {
    number: i32,
    word: []const u8,
    list: []const Expression,
    end: void,
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

            return Expression { .list = list.items };
        }

        if (token.?[0] == ')') {
            return Expression { .end = {} };
        }

        const number = std.fmt.parseInt(i32, token.?, 10) catch {
            var word = std.ArrayList(u8).init(allocator);
            word.appendSlice(token.?) catch unreachable;
            return Expression { .word = word.items };
        };
        return Expression{ .number = number };
    }
};

fn eval(store: std.StringHashMap(Expression), expression: Expression) Expression {
    return switch (expression) {
        .number => expression,
        .word => |word| store.get(word).?,
        .list => expression,
        .end => unreachable,
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
        .end => unreachable,
    }
}

pub fn main() void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    const store = std.StringHashMap(Expression).init(allocator);

    const tokenIterator = TokenIterator{
        .buffer = undefined,
        .wordIterator = null,
        .word = null,
    };

    var expressionIterator = ExpressionIterator{ .tokenIterator = tokenIterator };
    while (expressionIterator.next(allocator, stdin)) |expression| {
        print(stdout, eval(store, expression));
        stdout.writeByte('\n') catch unreachable;
    }
}
