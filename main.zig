const std = @import("std");

const Builtin = enum {
    quote,
    define,
    if_,
    plus,
    minus,
    times,
    equals,
    less,
    cons,
    car,
    cdr,
    length,
    null_,

    fn toString(self: Builtin) []const u8 {
        const builtins = [_][]const u8{ "quote", "define", "if", "+", "-", "*", "=", "<", "cons", "car", "cdr", "length", "null" };
        return builtins[@intFromEnum(self)];
    }
};

const Function = struct {
    name: []const u8,
    args: []const Expression,
    body: Expression,
};

const Expression = union(enum) {
    number: i32,
    word: []const u8,
    list: []const Expression,
    builtin: Builtin,
    function: *Function,
    end,
};

const Scope = struct {
    parent: ?*Scope,
    store: std.StringHashMap(Expression),

    fn init(allocator: anytype, parent: ?*Scope) Scope {
        return Scope{ .parent = parent, .store = std.StringHashMap(Expression).init(allocator) };
    }

    fn get(self: Scope, key: []const u8) ?Expression {
        return self.store.get(key) orelse if (self.parent) |parent| parent.get(key) else null;
    }

    fn put(self: *Scope, key: []const u8, value: Expression) void {
        self.store.put(key, value) catch unreachable;
    }
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

fn evalList(allocator: anytype, scope: *Scope, list: []const Expression) Expression {
    const operation = eval(allocator, scope, list[0]);
    switch (operation) {
        .function => |function| {
            var functionScope = Scope.init(allocator, scope);
            for (0.., function.args) |idx, arg| {
                functionScope.put(arg.word, eval(allocator, scope, list[idx + 1]));
            }
            return eval(allocator, &functionScope, function.body);
        },
        .builtin => |builtin| switch (builtin) {
            .quote => return list[1],
            .define => {
                switch (list[1]) {
                    .word => |word| {
                        scope.put(word, eval(allocator, scope, list[2]));
                        return list[1];
                    },
                    .list => |sublist| {
                        const function = allocator.create(Function) catch unreachable;
                        function.* = Function{ .name = sublist[0].word, .args = sublist[1..], .body = list[2] };
                        scope.put(sublist[0].word, Expression{ .function = function });
                        return sublist[0];
                    },
                    else => unreachable,
                }
            },
            .if_ => {
                const condition = eval(allocator, scope, list[1]);
                if (condition.number == 0) {
                    return eval(allocator, scope, list[3]);
                } else {
                    return eval(allocator, scope, list[2]);
                }
            },
            .plus => {
                var result: i32 = 0;
                for (list[1..]) |argument| {
                    const number = eval(allocator, scope, argument);
                    if (number == .number) {
                        result += number.number;
                    } else unreachable;
                }
                return Expression{ .number = result };
            },
            .minus => {
                var result: i32 = 0;
                for (0.., list[1..]) |idx, argument| {
                    const number = eval(allocator, scope, argument);
                    if (number == .number) {
                        if (list.len > 2 and idx == 0) {
                            result += number.number;
                        } else {
                            result -= number.number;
                        }
                    } else unreachable;
                }
                return Expression{ .number = result };
            },
            .times => {
                var result: i32 = 1;
                for (list[1..]) |argument| {
                    const number = eval(allocator, scope, argument);
                    if (number == .number) {
                        result *= number.number;
                    } else unreachable;
                }
                return Expression{ .number = result };
            },
            .equals => {
                const head = eval(allocator, scope, list[1]);
                if (head != .number) unreachable;
                for (list[2..]) |argument| {
                    const number = eval(allocator, scope, argument);
                    if (number == .number and number.number != head.number) {
                        return Expression{ .number = 0 };
                    }
                }
                return Expression{ .number = 1 };
            },
            .less => {
                const fst = eval(allocator, scope, list[1]);
                const snd = eval(allocator, scope, list[2]);
                if (fst != .number or snd != .number) unreachable;
                return Expression{ .number = if (fst.number < snd.number) 1 else 0 };
            },
            .cons => {
                const head = eval(allocator, scope, list[1]);
                const tail = eval(allocator, scope, list[2]);
                var newList = std.ArrayList(Expression).init(allocator);

                newList.append(head) catch unreachable;
                if (tail == .list) {
                    newList.appendSlice(tail.list) catch unreachable;
                } else unreachable;

                return Expression{ .list = newList.items };
            },
            .car => return eval(allocator, scope, list[1]).list[0],
            .cdr => return Expression{ .list = eval(allocator, scope, list[1]).list[1..] },
            .length => return Expression{ .number = @intCast(eval(allocator, scope, list[1]).list.len) },
            .null_ => return Expression{ .number = if (eval(allocator, scope, list[1]).list.len == 0) 1 else 0 },
        },
        else => unreachable,
    }
}

fn eval(allocator: anytype, scope: *Scope, expression: Expression) Expression {
    return switch (expression) {
        .number => expression,
        .word => |word| scope.get(word).?,
        .list => |list| if (list.len == 0) expression else evalList(allocator, scope, list),
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
        .builtin => |builtin| stdout.print("builtin {s}", .{builtin.toString()}) catch unreachable,
        .function => |function| stdout.print("function {s}", .{function.name}) catch unreachable,
        .end => unreachable,
    }
}

pub fn main() void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    var scope = Scope.init(allocator, null);
    for (0..@typeInfo(Builtin).Enum.fields.len) |idx| {
        const builtin: Builtin = @enumFromInt(idx);
        scope.put(builtin.toString(), Expression{ .builtin = builtin });
    }

    const tokenIterator = TokenIterator{
        .buffer = undefined,
        .wordIterator = null,
        .word = null,
    };

    var expressionIterator = ExpressionIterator{ .tokenIterator = tokenIterator };
    while (expressionIterator.next(allocator, stdin)) |expression| {
        print(stdout, eval(allocator, &scope, expression));
        stdout.writeByte('\n') catch unreachable;
    }
}
