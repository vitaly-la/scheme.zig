const std = @import("std");

const Builtin = enum {
    quote,
    define,
    if_,
    sum,
    minus,
    product,
    eq,
    lt,
    append,
    cons,
    car,
    cdr,
    length,
    null_,
    lambda,

    fn toString(self: Builtin) []const u8 {
        const builtins = [_][]const u8{ "quote", "define", "if", "+", "-", "*", "=", "<", "append", "cons", "car", "cdr", "length", "null", "lambda" };
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
    function: []const u8,

    fn init(allocator: anytype, parent: ?*const Scope, function: []const u8) Scope {
        return Scope{ .parent = parent, .store = std.StringHashMap(Expression).init(allocator), .function = function };
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

fn eval(allocator: anytype, scope: *Scope, expression_: Expression, final: bool) Expression {
    var expression = expression_;
    while (true) {
        switch (expression) {
            .number => return expression,
            .word => |word| return scope.get(word).?,
            .nil => return expression,
            .cons => {},
            else => unreachable,
        }
        const operation = eval(allocator, scope, expression.cons.car, false);
        var items = expression.cons.cdr;
        switch (operation) {
            .function => |function| {
                if (final and std.mem.eql(u8, function.name, scope.function)) {
                    var buffer: [128]Expression = undefined;
                    var arguments = function.arguments;
                    var idx: usize = 0;
                    while (arguments != .nil) : (arguments = arguments.cons.cdr) {
                        buffer[idx] = eval(allocator, scope, items.cons.car, false);
                        items = items.cons.cdr;
                        idx += 1;
                    }
                    arguments = function.arguments;
                    idx = 0;
                    while (arguments != .nil) : (arguments = arguments.cons.cdr) {
                        scope.put(arguments.cons.car.word, buffer[idx]);
                        idx += 1;
                    }
                    expression = function.body;
                    continue;
                }
                var functionScope = Scope.init(allocator, scope, function.name);
                var arguments = function.arguments;
                while (arguments != .nil) : (arguments = arguments.cons.cdr) {
                    functionScope.put(arguments.cons.car.word, eval(allocator, scope, items.cons.car, false));
                    items = items.cons.cdr;
                }
                return eval(allocator, &functionScope, function.body, true);
            },
            .builtin => |builtin| switch (builtin) {
                .quote => return items,
                .define => {
                    switch (items.cons.car) {
                        .word => |word| {
                            scope.put(word, eval(allocator, scope, items.cons.cdr.cons.car, false));
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
                    const condition = eval(allocator, scope, items.cons.car, false);
                    expression = if (condition.number == 0) items.cons.cdr.cons.cdr.cons.car else items.cons.cdr.cons.car;
                    continue;
                },
                .sum => {
                    var result: i32 = 0;
                    while (items != .nil) : (items = items.cons.cdr) {
                        const number = eval(allocator, scope, items.cons.car, false);
                        result += number.number;
                    }
                    return Expression{ .number = result };
                },
                .minus => {
                    var result: i32 = 0;
                    var fst = true;
                    while (items != .nil) : (items = items.cons.cdr) {
                        const number = eval(allocator, scope, items.cons.car, false);
                        if (fst and items.cons.cdr != .nil) {
                            result += number.number;
                        } else {
                            result -= number.number;
                        }
                        fst = false;
                    }
                    return Expression{ .number = result };
                },
                .product => {
                    var result: i32 = 1;
                    while (items != .nil) : (items = items.cons.cdr) {
                        const number = eval(allocator, scope, items.cons.car, false);
                        result *= number.number;
                    }
                    return Expression{ .number = result };
                },
                .eq => {
                    if (items == .nil) return Expression{ .number = 1 };
                    const head = eval(allocator, scope, items.cons.car, false);
                    items = items.cons.cdr;
                    while (items != .nil) : (items = items.cons.cdr) {
                        const number = eval(allocator, scope, items.cons.car, false);
                        if (number.number != head.number) {
                            return Expression{ .number = 0 };
                        }
                    }
                    return Expression{ .number = 1 };
                },
                .lt => {
                    const fst = eval(allocator, scope, items.cons.car, false);
                    const snd = eval(allocator, scope, items.cons.cdr.cons.car, false);
                    return Expression{ .number = if (fst.number < snd.number) 1 else 0 };
                },
                .append => {
                    var list = Expression{ .nil = {} };
                    var tail = &list;

                    var fst = eval(allocator, scope, items.cons.car, false);
                    while (fst != .nil) : (fst = fst.cons.cdr) {
                        const item = Expression{ .cons = allocator.create(Cons) catch unreachable };
                        item.cons.* = Cons{ .car = fst.cons.car, .cdr = Expression{ .nil = {} } };
                        tail.* = item;
                        tail = &item.cons.cdr;
                    }

                    const snd = eval(allocator, scope, items.cons.cdr.cons.car, false);
                    tail.* = snd;
                    return list;
                },
                .cons => {
                    const fst = eval(allocator, scope, items.cons.car, false);
                    const snd = eval(allocator, scope, items.cons.cdr.cons.car, false);
                    const list = Expression{ .cons = allocator.create(Cons) catch unreachable };
                    list.cons.* = Cons{ .car = fst, .cdr = snd };
                    return list;
                },
                .car => return eval(allocator, scope, items.cons.car, false).cons.car,
                .cdr => return eval(allocator, scope, items.cons.car, false).cons.cdr,
                .length => {
                    var list = eval(allocator, scope, items.cons.car, false);
                    var length: i32 = 0;
                    while (list != .nil) : (list = list.cons.cdr) {
                        length += 1;
                    }
                    return Expression{ .number = length };
                },
                .null_ => return Expression{ .number = if (eval(allocator, scope, items.cons.car, false) == .nil) 1 else 0 },
                .lambda => {
                    const function = allocator.create(Function) catch unreachable;
                    function.* = Function{ .name = "lambda", .arguments = items.cons.car, .body = items.cons.cdr.cons.car };
                    return Expression{ .function = function };
                },
            },
            else => unreachable,
        }
    }
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
                if (list == .cons) {
                    print(stdout, list.cons.car);
                    list = list.cons.cdr;
                    if (list == .nil) {
                        break;
                    }
                    stdout.writeByte(' ') catch unreachable;
                } else {
                    stdout.writeAll(". ") catch unreachable;
                    print(stdout, list);
                    break;
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

    var scope = Scope.init(allocator, null, "global");
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
        print(stdout, eval(allocator, &scope, expression, false));
        stdout.writeByte('\n') catch unreachable;
    }
}
