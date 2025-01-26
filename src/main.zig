const std = @import("std");

const GLOBAL = -1;

const BUILTINS = [_][]const u8{
    "+",
    "-",
    "*",
    "quotient",
    "modulo",
    "abs",
    "min",
    "max",
    "and",
    "or",
    "not",
    "quote",
    "define",
    "if",
    "=",
    "<",
    "<=",
    ">",
    ">=",
    "begin",
    "list",
    "append",
    "map",
    "filter",
    "cons",
    "car",
    "cdr",
    "length",
    "null?",
    "lambda",
};

const Cons = struct {
    car: Expression,
    cdr: Expression,

    inline fn singleton(arg: Expression) Cons {
        return Cons{ .car = arg, .cdr = Expression.nil() };
    }

    inline fn pair(fst: Expression, snd: Expression) Cons {
        return Cons{ .car = fst, .cdr = snd };
    }
};

const Builtin = enum {
    sum,
    minus,
    product,
    quotient,
    modulo,
    abs,
    min,
    max,
    and_,
    or_,
    not,
    quote,
    define,
    if_,
    eq,
    lt,
    le,
    gt,
    ge,
    begin,
    list,
    append,
    map,
    filter,
    cons,
    car,
    cdr,
    length,
    null_,
    lambda,

    inline fn toString(self: Builtin) []const u8 {
        return BUILTINS[@intFromEnum(self)];
    }
};

const Function = struct {
    args: Expression,
    body: Expression,
    name: isize,

    inline fn new(args: Expression, body: Expression, name: isize) Function {
        return Function{ .args = args, .body = body, .name = name };
    }
};

const Expression = union(enum) {
    number: isize,
    word: isize,
    nil,
    cons: *Cons,
    builtin: Builtin,
    function: *const Function,
    end,

    inline fn number(arg: isize) Expression {
        return Expression{ .number = arg };
    }

    inline fn word(arg: isize) Expression {
        return Expression{ .word = arg };
    }

    inline fn nil() Expression {
        return Expression{ .nil = {} };
    }

    inline fn cons(arg: *Cons) Expression {
        return Expression{ .cons = arg };
    }

    inline fn builtin(arg: Builtin) Expression {
        return Expression{ .builtin = arg };
    }

    inline fn function(arg: *const Function) Expression {
        return Expression{ .function = arg };
    }

    inline fn end() Expression {
        return Expression{ .end = {} };
    }
};

const Scope = struct {
    store: std.AutoHashMap(isize, Expression),
    parent: ?*Scope,
    function: isize,

    fn init(allocator: anytype, parent: ?*Scope, function: isize) Scope {
        return Scope{
            .store = std.AutoHashMap(isize, Expression).init(allocator),
            .parent = parent,
            .function = function,
        };
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
        return SymbolTable{
            .table = std.StringHashMap(isize).init(allocator),
            .reverse = std.AutoHashMap(isize, []const u8).init(allocator),
            .idx = 0,
        };
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

    fn getLambda(self: *SymbolTable, allocator: anytype) isize {
        const lambda = std.fmt.allocPrint(allocator, "lambda-{d}", .{self.idx}) catch unreachable;
        self.table.put(lambda, self.idx) catch unreachable;
        self.reverse.put(self.idx, lambda) catch unreachable;
        self.idx += 1;
        return self.idx - 1;
    }

    fn getScope(self: *SymbolTable) isize {
        self.idx += 1;
        return self.idx - 1;
    }

    fn str(self: SymbolTable, key: isize) []const u8 {
        return self.reverse.get(key).?;
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
                if (self.word == null or self.word.?[0] == ';') {
                    self.wordIterator = null;
                    self.word = null;
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
    symbols: *SymbolTable,

    fn next(self: *ExpressionIterator, allocator: anytype, stdin: anytype) ?Expression {
        const token = self.tokenIterator.next(stdin);
        if (token == null) {
            return null;
        }

        if (token.?[0] == '(') {
            var arrayList = std.ArrayList(Cons).init(allocator);
            while (self.next(allocator, stdin)) |subexpression| {
                if (subexpression == .end) {
                    break;
                }
                arrayList.append(Cons.singleton(subexpression)) catch unreachable;
            }
            if (arrayList.items.len > 0) {
                for (1.., arrayList.items[1..]) |idx, *item| {
                    arrayList.items[idx - 1].cdr = Expression.cons(item);
                }
                return Expression.cons(&arrayList.items[0]);
            }
            return Expression.nil();
        }

        if (token.?[0] == ')') {
            return Expression.end();
        }

        if (token.?[0] == '\'') {
            const subexpression = self.next(allocator, stdin).?;
            const quote = allocator.alloc(Cons, 2) catch unreachable;
            quote[0] = Cons.pair(Expression.word(self.symbols.get("quote")), Expression.cons(&quote[1]));
            quote[1] = Cons.singleton(subexpression);
            return Expression.cons(&quote[0]);
        }

        const number = std.fmt.parseInt(isize, token.?, 10) catch {
            const word = allocator.alloc(u8, token.?.len) catch unreachable;
            std.mem.copyForwards(u8, word, token.?);
            return Expression.word(self.symbols.get(word));
        };
        return Expression.number(number);
    }
};

fn eval(allocator: anytype, symbols: *SymbolTable, scope_: *Scope, expression_: Expression, final_: bool) Expression {
    var scope = scope_;
    var expression = expression_;
    var final = final_;
    var free = false;
    const ret = ret: while (true) {
        switch (expression) {
            .number => break :ret expression,
            .word => |word| break :ret scope.get(word).?,
            .cons => {},
            else => unreachable,
        }
        const operation = eval(allocator, symbols, scope, expression.cons.car, false);
        var args = expression.cons.cdr;
        switch (operation) {
            .function => |function| {
                var buffer: [16]Expression = undefined;
                var fargs = function.args;
                var idx: usize = 0;
                while (fargs != .nil) : (fargs = fargs.cons.cdr) {
                    buffer[idx] = eval(allocator, symbols, scope, args.cons.car, false);
                    args = args.cons.cdr;
                    idx += 1;
                }
                if (!final or function.name != scope.function) {
                    const functionScope = allocator.create(Scope) catch unreachable;
                    functionScope.* = Scope.init(allocator, scope, function.name);
                    scope = functionScope;
                    final = true;
                    free = true;
                }
                fargs = function.args;
                idx = 0;
                while (fargs != .nil) : (fargs = fargs.cons.cdr) {
                    scope.put(fargs.cons.car.word, buffer[idx]);
                    idx += 1;
                }
                expression = function.body;
                continue;
            },
            .builtin => |builtin| switch (builtin) {
                .sum, .product, .and_, .or_ => {
                    var result: isize = switch (builtin) {
                        .sum => 0,
                        .product => 1,
                        .and_ => 1,
                        else => 0,
                    };
                    while (args != .nil) : (args = args.cons.cdr) {
                        const arg = eval(allocator, symbols, scope, args.cons.car, false).number;
                        switch (builtin) {
                            .sum => result += arg,
                            .product => result *= arg,
                            .and_ => if (arg == 0) {
                                result = 0;
                                break;
                            },
                            else => if (arg != 0) {
                                result = 1;
                                break;
                            },
                        }
                    }
                    break :ret Expression.number(result);
                },
                .minus => {
                    var result: isize = 0;
                    var fst = true;
                    while (args != .nil) : (args = args.cons.cdr) {
                        const number = eval(allocator, symbols, scope, args.cons.car, false);
                        if (fst and args.cons.cdr != .nil) {
                            result += number.number;
                        } else {
                            result -= number.number;
                        }
                        fst = false;
                    }
                    break :ret Expression.number(result);
                },
                .quotient, .modulo => {
                    const fst = eval(allocator, symbols, scope, args.cons.car, false);
                    const snd = eval(allocator, symbols, scope, args.cons.cdr.cons.car, final);
                    if (builtin == .quotient) {
                        break :ret Expression.number(@divTrunc(fst.number, snd.number));
                    } else {
                        break :ret Expression.number(@mod(fst.number, snd.number));
                    }
                },
                .abs, .not => {
                    const arg = eval(allocator, symbols, scope, args.cons.car, final).number;
                    if (builtin == .abs) {
                        break :ret Expression.number(if (arg < 0) -arg else arg);
                    } else {
                        break :ret Expression.number(if (arg == 0) 1 else 0);
                    }
                },
                .min, .max => {
                    var arg = eval(allocator, symbols, scope, args.cons.car, false).number;
                    args = args.cons.cdr;
                    while (args != .nil) : (args = args.cons.cdr) {
                        const other = eval(allocator, symbols, scope, args.cons.car, false).number;
                        if (builtin == .min) {
                            arg = if (other < arg) other else arg;
                        } else {
                            arg = if (other > arg) other else arg;
                        }
                    }
                    break :ret Expression.number(arg);
                },
                .quote => break :ret args.cons.car,
                .define => {
                    switch (args.cons.car) {
                        .word => |word| {
                            scope.put(word, eval(allocator, symbols, scope, args.cons.cdr.cons.car, false));
                            break :ret args.cons.car;
                        },
                        .cons => |definition| {
                            const function = allocator.create(Function) catch unreachable;
                            function.* = Function.new(definition.cdr, args.cons.cdr.cons.car, definition.car.word);
                            scope.put(definition.car.word, Expression.function(function));
                            break :ret definition.car;
                        },
                        else => unreachable,
                    }
                },
                .if_ => {
                    const condition = eval(allocator, symbols, scope, args.cons.car, false);
                    if (condition.number == 0) {
                        expression = args.cons.cdr.cons.cdr.cons.car;
                    } else {
                        expression = args.cons.cdr.cons.car;
                    }
                    continue;
                },
                .eq => {
                    if (args == .nil) break :ret Expression.number(1);
                    const head = eval(allocator, symbols, scope, args.cons.car, false);
                    args = args.cons.cdr;
                    while (args != .nil) : (args = args.cons.cdr) {
                        const number = eval(allocator, symbols, scope, args.cons.car, false);
                        if (number.number != head.number) {
                            break :ret Expression.number(0);
                        }
                    }
                    break :ret Expression.number(1);
                },
                .lt, .le, .gt, .ge => {
                    const fst = eval(allocator, symbols, scope, args.cons.car, false);
                    const snd = eval(allocator, symbols, scope, args.cons.cdr.cons.car, final);
                    break :ret switch (builtin) {
                        .lt => Expression.number(if (fst.number < snd.number) 1 else 0),
                        .le => Expression.number(if (fst.number <= snd.number) 1 else 0),
                        .gt => Expression.number(if (fst.number > snd.number) 1 else 0),
                        else => Expression.number(if (fst.number >= snd.number) 1 else 0),
                    };
                },
                .begin => {
                    var innerScope = Scope.init(allocator, scope, symbols.getScope());
                    defer innerScope.deinit();
                    while (args != .nil) : (args = args.cons.cdr) {
                        if (args.cons.cdr == .nil) {
                            break :ret eval(allocator, symbols, &innerScope, args.cons.car, true);
                        } else {
                            _ = eval(allocator, symbols, &innerScope, args.cons.car, false);
                        }
                    }
                },
                .list => {
                    if (args == .nil) {
                        break :ret args;
                    }
                    var argsPtr = args;
                    var length: usize = 0;
                    while (argsPtr != .nil) : (argsPtr = argsPtr.cons.cdr) {
                        length += 1;
                    }
                    const list = allocator.alloc(Cons, length) catch unreachable;
                    for (0.., list) |idx, *item| {
                        if (idx < list.len - 1) {
                            item.* = Cons.pair(eval(allocator, symbols, scope, args.cons.car, false), Expression.cons(&list[idx + 1]));
                        } else {
                            item.* = Cons.singleton(eval(allocator, symbols, scope, args.cons.car, final));
                        }
                        args = args.cons.cdr;
                    }
                    break :ret Expression.cons(&list[0]);
                },
                .append => {
                    if (args == .nil) {
                        break :ret args;
                    }
                    if (args.cons.cdr == .nil) {
                        break :ret eval(allocator, symbols, scope, args.cons.car, final);
                    }
                    var arrayList = std.ArrayList(Cons).init(allocator);
                    while (args.cons.cdr != .nil) : (args = args.cons.cdr) {
                        var arg = eval(allocator, symbols, scope, args.cons.car, false);
                        while (arg != .nil) : (arg = arg.cons.cdr) {
                            arrayList.append(Cons.singleton(arg.cons.car)) catch unreachable;
                        }
                    }
                    const last = eval(allocator, symbols, scope, args.cons.car, final);
                    if (arrayList.items.len == 0) {
                        arrayList.deinit();
                        break :ret last;
                    }
                    for (1.., arrayList.items[1..]) |idx, *item| {
                        arrayList.items[idx - 1].cdr = Expression.cons(item);
                    }
                    arrayList.items[arrayList.items.len - 1].cdr = last;
                    break :ret Expression.cons(&arrayList.items[0]);
                },
                .map => {
                    var arg = eval(allocator, symbols, scope, args.cons.cdr.cons.car, false);
                    if (arg == .nil) {
                        break :ret arg;
                    }
                    var argPtr = arg;
                    var length: usize = 0;
                    while (argPtr != .nil) : (argPtr = argPtr.cons.cdr) {
                        length += 1;
                    }
                    var list = allocator.alloc(Cons, length) catch unreachable;
                    for (0.., list) |idx, *item| {
                        var argument = Cons.singleton(arg.cons.car);
                        var call = Cons.pair(args.cons.car, Expression.cons(&argument));
                        if (idx < list.len - 1) {
                            item.* = Cons.pair(eval(allocator, symbols, scope, Expression.cons(&call), false), Expression.cons(&list[idx + 1]));
                        } else {
                            item.* = Cons.pair(eval(allocator, symbols, scope, Expression.cons(&call), final), Expression.nil());
                        }
                        arg = arg.cons.cdr;
                    }
                    break :ret Expression.cons(&list[0]);
                },
                .filter => {
                    var arg = eval(allocator, symbols, scope, args.cons.cdr.cons.car, false);
                    if (arg == .nil) {
                        break :ret arg;
                    }
                    var arrayList = std.ArrayList(Cons).init(allocator);
                    while (arg != .nil) : (arg = arg.cons.cdr) {
                        var argument = Cons.singleton(arg.cons.car);
                        var call = Cons.pair(args.cons.car, Expression.cons(&argument));
                        const pred = eval(allocator, symbols, scope, Expression.cons(&call), if (arg.cons.cdr != .nil) false else final);
                        if (pred.number != 0) {
                            arrayList.append(argument) catch unreachable;
                        }
                    }
                    if (arrayList.items.len == 0) {
                        arrayList.deinit();
                        break :ret Expression.nil();
                    }
                    for (1.., arrayList.items[1..]) |idx, *item| {
                        arrayList.items[idx - 1].cdr = Expression.cons(item);
                    }
                    break :ret Expression.cons(&arrayList.items[0]);
                },
                .cons => {
                    const fst = eval(allocator, symbols, scope, args.cons.car, false);
                    const snd = eval(allocator, symbols, scope, args.cons.cdr.cons.car, final);
                    const pair = allocator.create(Cons) catch unreachable;
                    pair.* = Cons.pair(fst, snd);
                    break :ret Expression.cons(pair);
                },
                .car => break :ret eval(allocator, symbols, scope, args.cons.car, final).cons.car,
                .cdr => break :ret eval(allocator, symbols, scope, args.cons.car, final).cons.cdr,
                .length => {
                    var list = eval(allocator, symbols, scope, args.cons.car, final);
                    var length: isize = 0;
                    while (list != .nil) : (list = list.cons.cdr) {
                        length += 1;
                    }
                    break :ret Expression.number(length);
                },
                .null_ => break :ret Expression{
                    .number = if (eval(allocator, symbols, scope, args.cons.car, final) == .nil) 1 else 0,
                },
                .lambda => {
                    const function = allocator.create(Function) catch unreachable;
                    function.* = Function.new(args.cons.car, args.cons.cdr.cons.car, symbols.getLambda(allocator));
                    break :ret Expression.function(function);
                },
            },
            else => unreachable,
        }
    };
    if (free) {
        const parent = scope.parent.?;
        scope.deinit();
        allocator.destroy(scope);
        scope = parent;
    }
    return ret;
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
        .builtin => |builtin| stdout.print("#<procedure {s}>", .{builtin.toString()}) catch unreachable,
        .function => |function| stdout.print("#<procedure {s}>", .{symbols.str(function.name)}) catch unreachable,
        .end => unreachable,
    }
}

pub fn main() void {
    const allocator = std.heap.c_allocator;
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    var symbols = SymbolTable.init(allocator);

    var scope = Scope.init(allocator, null, GLOBAL);
    for (0..@typeInfo(Builtin).Enum.fields.len) |idx| {
        const builtin: Builtin = @enumFromInt(idx);
        const id = symbols.get(builtin.toString());
        scope.put(id, Expression.builtin(builtin));
    }

    const tokenIterator = TokenIterator{
        .buffer = undefined,
        .wordIterator = null,
        .word = null,
    };

    var expressionIterator = ExpressionIterator{ .tokenIterator = tokenIterator, .symbols = &symbols };
    stdout.writeAll("vitaly-la/scheme.zig version 1.0.0\n\n> ") catch unreachable;
    while (expressionIterator.next(allocator, stdin)) |expression| {
        print(stdout, eval(allocator, &symbols, &scope, expression, false), symbols);
        stdout.writeAll("\n> ") catch unreachable;
    }
    stdout.writeAll("\n") catch unreachable;
}
