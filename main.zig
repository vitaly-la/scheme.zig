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
    arguments: Expression,
    body: Expression,
};

const Cons = struct {
    car: Expression,
    cdr: Expression,
};

const Expression = union(enum) {
    number: i32,
    word: []const u8,
    nil,
    cons: *Cons,
    builtin: Builtin,
    function: *const Function,
    end,
};

const Scope = struct {
    parent: ?*const Scope,
    store: std.StringHashMap(Expression),

    fn init(allocator: anytype, parent: ?*const Scope) Scope {
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
            var list = Expression{ .nil = {} };
            var tail = &list;

            while (self.next(allocator, stdin)) |subexpression| {
                if (subexpression == .end) {
                    break;
                }
                const item = Expression{ .cons = allocator.create(Cons) catch unreachable };
                item.cons.* = Cons{ .car = subexpression, .cdr = Expression{ .nil = {} } };
                tail.* = item;
                tail = &item.cons.cdr;
            }

            return list;
        }

        if (token.?[0] == ')') {
            return Expression{ .end = {} };
        }

        if (token.?[0] == '\'') {
            const cons = Expression{ .cons = allocator.create(Cons) catch unreachable };
            const subexpression = self.next(allocator, stdin).?;
            cons.cons.* = Cons{ .car = Expression{ .word = "quote" }, .cdr = subexpression };
            return cons;
        }

        const number = std.fmt.parseInt(i32, token.?, 10) catch {
            var word = std.ArrayList(u8).init(allocator);
            word.appendSlice(token.?) catch unreachable;
            return Expression{ .word = word.items };
        };
        return Expression{ .number = number };
    }
};

fn evalCons(allocator: anytype, scope: *Scope, cons: Cons) Expression {
    const operation = eval(allocator, scope, cons.car);
    var items = cons.cdr;
    switch (operation) {
        .function => |function| {
            var functionScope = Scope.init(allocator, scope);
            var arguments = function.arguments;
            while (arguments != .nil) {
                functionScope.put(arguments.cons.car.word, eval(allocator, scope, items.cons.car));
                arguments = arguments.cons.cdr;
                items = items.cons.cdr;
            }
            return eval(allocator, &functionScope, function.body);
        },
        .builtin => |builtin| switch (builtin) {
            .quote => return items,
            .define => {
                switch (items.cons.car) {
                    .word => |word| {
                        scope.put(word, eval(allocator, scope, items.cons.cdr.cons.car));
                        return items.cons.car;
                    },
                    .cons => |definition| {
                        const function = allocator.create(Function) catch unreachable;
                        function.* = Function{ .name = definition.car.word, .arguments = definition.cdr, .body = items.cons.cdr.cons.car };
                        scope.put(definition.car.word, Expression{ .function = function });
                        return definition.car;
                    },
                    else => unreachable,
                }
            },
            .if_ => {
                const condition = eval(allocator, scope, items.cons.car);
                if (condition.number == 0) {
                    return eval(allocator, scope, items.cons.cdr.cons.cdr.cons.car);
                } else {
                    return eval(allocator, scope, items.cons.cdr.cons.car);
                }
            },
            .plus => {
                var result: i32 = 0;
                while (items != .nil) : (items = items.cons.cdr) {
                    const number = eval(allocator, scope, items.cons.car);
                    result += number.number;
                }
                return Expression{ .number = result };
            },
            .minus => {
                var result: i32 = 0;
                var fst = true;
                while (items != .nil) : (items = items.cons.cdr) {
                    const number = eval(allocator, scope, items.cons.car);
                    if (fst and items.cons.cdr != .nil) {
                        result += number.number;
                    } else {
                        result -= number.number;
                    }
                    fst = false;
                }
                return Expression{ .number = result };
            },
            .times => {
                var result: i32 = 1;
                while (items != .nil) : (items = items.cons.cdr) {
                    const number = eval(allocator, scope, items.cons.car);
                    result *= number.number;
                }
                return Expression{ .number = result };
            },
            .equals => {
                const head = eval(allocator, scope, items.cons.car);
                items = items.cons.cdr;
                while (items != .nil) : (items = items.cons.cdr) {
                    const number = eval(allocator, scope, items.cons.car);
                    if (number.number != head.number) {
                        return Expression{ .number = 0 };
                    }
                }
                return Expression{ .number = 1 };
            },
            .less => {
                const fst = eval(allocator, scope, items.cons.car);
                const snd = eval(allocator, scope, items.cons.cdr.cons.car);
                return Expression{ .number = if (fst.number < snd.number) 1 else 0 };
            },
            .cons => {
                const fst = eval(allocator, scope, items.cons.car);
                const snd = eval(allocator, scope, items.cons.cdr.cons.car);
                const expression = Expression{ .cons = allocator.create(Cons) catch unreachable };
                expression.cons.* = Cons{ .car = fst, .cdr = snd };
                return expression;
            },
            .car => return eval(allocator, scope, items.cons.car).cons.car,
            .cdr => return eval(allocator, scope, items.cons.car).cons.cdr,
            .length => {
                var list = eval(allocator, scope, items.cons.car);
                var length: i32 = 0;
                while (list != .nil) : (list = list.cons.cdr) {
                    length += 1;
                }
                return Expression{ .number = length };
            },
            .null_ => return Expression{ .number = if (eval(allocator, scope, items.cons.car) == .nil) 1 else 0 },
        },
        else => unreachable,
    }
}

fn eval(allocator: anytype, scope: *Scope, expression: Expression) Expression {
    return switch (expression) {
        .number => expression,
        .word => |word| scope.get(word).?,
        .nil => expression,
        .cons => |cons| evalCons(allocator, scope, cons.*),
        else => unreachable,
    };
}

fn print(stdout: anytype, expression: Expression) void {
    switch (expression) {
        .number => |number| stdout.print("{d}", .{number}) catch unreachable,
        .word => |word| stdout.print("{s}", .{word}) catch unreachable,
        .nil => stdout.writeAll("()") catch unreachable,
        .cons => {
            stdout.writeByte('(') catch unreachable;
            var list = expression;
            while (true) {
                print(stdout, list.cons.car);
                list = list.cons.cdr;
                if (list == .nil) {
                    break;
                }
                stdout.writeByte(' ') catch unreachable;
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
