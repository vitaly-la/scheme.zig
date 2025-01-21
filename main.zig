const std = @import("std");

const GLOBAL = -1;

const Cons = struct {
    car: Expression,
    cdr: Expression,
};

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
    args: Expression,
    body: Expression,
    name: isize,
};

const Expression = union(enum) {
    number: isize,
    word: isize,
    nil,
    cons: *Cons,
    builtin: Builtin,
    function: *const Function,
    end,
};

const Scope = struct {
    store: std.AutoHashMap(isize, Expression),
    parent: ?*const Scope,
    function: isize,

    fn init(allocator: anytype, parent: ?*const Scope, function: isize) Scope {
        return Scope{ .store = std.AutoHashMap(isize, Expression).init(allocator), .parent = parent, .function = function };
    }

    fn deinit(self: *Scope) void {
        self.store.deinit();
    }

    fn get(self: Scope, key: isize) ?Expression {
        return self.store.get(key) orelse if (self.parent) |parent| parent.get(key) else null;
    }

    fn put(self: *Scope, key: isize, value: Expression) void {
        self.store.put(key, value) catch unreachable;
    }
};

const SymbolTable = struct {
    table: std.StringHashMap(isize),
    reverse: std.AutoHashMap(isize, []const u8),
    idx: isize,

    fn init(allocator: anytype) SymbolTable {
        return SymbolTable{ .table = std.StringHashMap(isize).init(allocator), .reverse = std.AutoHashMap(isize, []const u8).init(allocator), .idx = 0 };
    }

    fn get(self: *SymbolTable, key: []const u8) isize {
        if (self.table.get(key)) |value| {
            return value;
        } else {
            self.table.put(key, self.idx) catch unreachable;
            self.reverse.put(self.idx, key) catch unreachable;
            self.idx += 1;
            return self.idx - 1;
        }
    }

    fn str(self: SymbolTable, key: isize) []const u8 {
        return self.reverse.get(key).?;
    }

    fn getLambda(self: *SymbolTable, allocator: anytype) isize {
        const lambda = std.fmt.allocPrint(allocator, "lambda-{d}", .{self.idx}) catch unreachable;
        self.table.put(lambda, self.idx) catch unreachable;
        self.reverse.put(self.idx, lambda) catch unreachable;
        self.idx += 1;
        return self.idx - 1;
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
    symbols: SymbolTable,

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
            const subexpression = self.next(allocator, stdin).?;
            const item = Expression{ .cons = allocator.create(Cons) catch unreachable };
            item.cons.* = Cons{ .car = subexpression, .cdr = Expression{ .nil = {} } };
            const quote = Expression{ .cons = allocator.create(Cons) catch unreachable };
            quote.cons.* = Cons{ .car = Expression{ .word = self.symbols.get("quote") }, .cdr = item };
            return quote;
        }

        const number = std.fmt.parseInt(isize, token.?, 10) catch {
            const word = allocator.alloc(u8, token.?.len) catch unreachable;
            std.mem.copyForwards(u8, word, token.?);
            return Expression{ .word = self.symbols.get(word) };
        };
        return Expression{ .number = number };
    }
};

fn eval(allocator: anytype, scope: *Scope, symbols: *SymbolTable, expression_: Expression, final: bool) Expression {
    var expression = expression_;
    while (true) {
        switch (expression) {
            .number => return expression,
            .word => |word| return scope.get(word).?,
            .nil => return expression,
            .cons => {},
            else => unreachable,
        }
        const operation = eval(allocator, scope, symbols, expression.cons.car, false);
        var args = expression.cons.cdr;
        switch (operation) {
            .function => |function| {
                var buffer: [16]Expression = undefined;
                var fargs = function.args;
                var idx: usize = 0;
                while (fargs != .nil) : (fargs = fargs.cons.cdr) {
                    buffer[idx] = eval(allocator, scope, symbols, args.cons.car, false);
                    args = args.cons.cdr;
                    idx += 1;
                }
                fargs = function.args;
                idx = 0;
                if (final and function.name == scope.function) {
                    while (fargs != .nil) : (fargs = fargs.cons.cdr) {
                        scope.put(fargs.cons.car.word, buffer[idx]);
                        idx += 1;
                    }
                    expression = function.body;
                    continue;
                }
                var functionScope = Scope.init(allocator, scope, function.name);
                defer functionScope.deinit();
                while (fargs != .nil) : (fargs = fargs.cons.cdr) {
                    functionScope.put(fargs.cons.car.word, buffer[idx]);
                    idx += 1;
                }
                return eval(allocator, &functionScope, symbols, function.body, true);
            },
            .builtin => |builtin| switch (builtin) {
                .quote => return args.cons.car,
                .define => {
                    switch (args.cons.car) {
                        .word => |word| {
                            scope.put(word, eval(allocator, scope, symbols, args.cons.cdr.cons.car, false));
                            return args.cons.car;
                        },
                        .cons => |definition| {
                            const function = allocator.create(Function) catch unreachable;
                            function.* = Function{ .args = definition.cdr, .body = args.cons.cdr.cons.car, .name = definition.car.word };
                            scope.put(definition.car.word, Expression{ .function = function });
                            return definition.car;
                        },
                        else => unreachable,
                    }
                },
                .if_ => {
                    const condition = eval(allocator, scope, symbols, args.cons.car, false);
                    expression = if (condition.number == 0) args.cons.cdr.cons.cdr.cons.car else args.cons.cdr.cons.car;
                    continue;
                },
                .sum => {
                    var result: isize = 0;
                    while (args != .nil) : (args = args.cons.cdr) {
                        const number = eval(allocator, scope, symbols, args.cons.car, false);
                        result += number.number;
                    }
                    return Expression{ .number = result };
                },
                .minus => {
                    var result: isize = 0;
                    var fst = true;
                    while (args != .nil) : (args = args.cons.cdr) {
                        const number = eval(allocator, scope, symbols, args.cons.car, false);
                        if (fst and args.cons.cdr != .nil) {
                            result += number.number;
                        } else {
                            result -= number.number;
                        }
                        fst = false;
                    }
                    return Expression{ .number = result };
                },
                .product => {
                    var result: isize = 1;
                    while (args != .nil) : (args = args.cons.cdr) {
                        const number = eval(allocator, scope, symbols, args.cons.car, false);
                        result *= number.number;
                    }
                    return Expression{ .number = result };
                },
                .eq => {
                    if (args == .nil) return Expression{ .number = 1 };
                    const head = eval(allocator, scope, symbols, args.cons.car, false);
                    args = args.cons.cdr;
                    while (args != .nil) : (args = args.cons.cdr) {
                        const number = eval(allocator, scope, symbols, args.cons.car, false);
                        if (number.number != head.number) {
                            return Expression{ .number = 0 };
                        }
                    }
                    return Expression{ .number = 1 };
                },
                .lt => {
                    const fst = eval(allocator, scope, symbols, args.cons.car, false);
                    const snd = eval(allocator, scope, symbols, args.cons.cdr.cons.car, false);
                    return Expression{ .number = if (fst.number < snd.number) 1 else 0 };
                },
                .append => {
                    var list = Expression{ .nil = {} };
                    var tail = &list;

                    var fst = eval(allocator, scope, symbols, args.cons.car, false);
                    while (fst != .nil) : (fst = fst.cons.cdr) {
                        const item = Expression{ .cons = allocator.create(Cons) catch unreachable };
                        item.cons.* = Cons{ .car = fst.cons.car, .cdr = Expression{ .nil = {} } };
                        tail.* = item;
                        tail = &item.cons.cdr;
                    }

                    const snd = eval(allocator, scope, symbols, args.cons.cdr.cons.car, final);
                    tail.* = snd;
                    return list;
                },
                .cons => {
                    const fst = eval(allocator, scope, symbols, args.cons.car, false);
                    const snd = eval(allocator, scope, symbols, args.cons.cdr.cons.car, final);
                    const list = Expression{ .cons = allocator.create(Cons) catch unreachable };
                    list.cons.* = Cons{ .car = fst, .cdr = snd };
                    return list;
                },
                .car => return eval(allocator, scope, symbols, args.cons.car, final).cons.car,
                .cdr => return eval(allocator, scope, symbols, args.cons.car, final).cons.cdr,
                .length => {
                    var list = eval(allocator, scope, symbols, args.cons.car, final);
                    var length: isize = 0;
                    while (list != .nil) : (list = list.cons.cdr) {
                        length += 1;
                    }
                    return Expression{ .number = length };
                },
                .null_ => return Expression{ .number = if (eval(allocator, scope, symbols, args.cons.car, final) == .nil) 1 else 0 },
                .lambda => {
                    const function = allocator.create(Function) catch unreachable;
                    function.* = Function{ .args = args.cons.car, .body = args.cons.cdr.cons.car, .name = symbols.getLambda(allocator) };
                    return Expression{ .function = function };
                },
            },
            else => unreachable,
        }
    }
}

fn print(stdout: anytype, expression: Expression, symbols: SymbolTable) void {
    switch (expression) {
        .number => |number| stdout.print("{d}", .{number}) catch unreachable,
        .word => |word| stdout.print("{s}", .{symbols.str(word)}) catch unreachable,
        .nil => stdout.writeAll("()") catch unreachable,
        .cons => {
            stdout.writeByte('(') catch unreachable;
            var list = expression;
            while (list == .cons) : (list = list.cons.cdr) {
                print(stdout, list.cons.car, symbols);
                if (list.cons.cdr != .nil) {
                    stdout.writeByte(' ') catch unreachable;
                }
            }
            if (list != .nil) {
                stdout.writeAll(". ") catch unreachable;
                print(stdout, list, symbols);
            }
            stdout.writeByte(')') catch unreachable;
        },
        .builtin => |builtin| stdout.print("builtin {s}", .{builtin.toString()}) catch unreachable,
        .function => |function| stdout.print("function {s}", .{symbols.str(function.name)}) catch unreachable,
        .end => unreachable,
    }
}

pub fn main() void {
    const buffer = std.heap.page_allocator.alloc(u8, 16 * 1024 * 1024) catch unreachable;
    defer std.heap.page_allocator.free(buffer);

    var fba = std.heap.FixedBufferAllocator.init(buffer);
    const allocator = fba.allocator();

    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    var symbols = SymbolTable.init(allocator);

    var scope = Scope.init(allocator, null, GLOBAL);
    for (0..@typeInfo(Builtin).Enum.fields.len) |idx| {
        const builtin: Builtin = @enumFromInt(idx);
        const id = symbols.get(builtin.toString());
        scope.put(id, Expression{ .builtin = builtin });
    }

    const tokenIterator = TokenIterator{
        .buffer = undefined,
        .wordIterator = null,
        .word = null,
    };

    var expressionIterator = ExpressionIterator{ .tokenIterator = tokenIterator, .symbols = symbols };
    while (expressionIterator.next(allocator, stdin)) |expression| {
        print(stdout, eval(allocator, &scope, &symbols, expression, false), symbols);
        stdout.writeByte('\n') catch unreachable;
    }
}
