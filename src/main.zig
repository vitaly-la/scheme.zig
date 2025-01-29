const std = @import("std");

const MAXWIDTH = 128;
const MAXARGS = 16;
const GLOBAL_SCOPE = -1;

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
    "cond",
    "=",
    "<",
    "<=",
    ">",
    ">=",
    "begin",
    "let",
    "let*",
    "list",
    "append",
    "map",
    "filter",
    "cons",
    "car",
    "cdr",
    "cadr",
    "cddr",
    "length",
    "null?",
    "lambda",
};

const Cons = struct {
    car: Expression,
    cdr: Expression,

    fn singleton(arg: Expression) Cons {
        return Cons{ .car = arg, .cdr = Expression.nil() };
    }

    fn pair(fst: Expression, snd: Expression) Cons {
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
    cond,
    eq,
    lt,
    le,
    gt,
    ge,
    begin,
    let,
    letStar,
    list,
    append,
    map,
    filter,
    cons,
    car,
    cdr,
    cadr,
    cddr,
    length,
    null_,
    lambda,

    fn toString(self: Builtin) []const u8 {
        return BUILTINS[@intFromEnum(self)];
    }
};

const Function = struct {
    args: Expression,
    body: Expression,
    name: isize,

    fn new(args: Expression, body: Expression, name: isize) Function {
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
    empty,
    end,

    fn number(arg: isize) Expression {
        return Expression{ .number = arg };
    }

    fn word(arg: isize) Expression {
        return Expression{ .word = arg };
    }

    fn nil() Expression {
        return Expression{ .nil = {} };
    }

    fn cons(arg: *Cons) Expression {
        return Expression{ .cons = arg };
    }

    fn builtin(arg: Builtin) Expression {
        return Expression{ .builtin = arg };
    }

    fn function(arg: *const Function) Expression {
        return Expression{ .function = arg };
    }

    fn empty() Expression {
        return Expression{ .empty = {} };
    }

    fn end() Expression {
        return Expression{ .end = {} };
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

    fn get(self: *SymbolTable, key: []const u8) !isize {
        if (self.table.get(key)) |value| {
            return value;
        } else {
            try self.table.put(key, self.idx);
            try self.reverse.put(self.idx, key);
            self.idx += 1;
            return self.idx - 1;
        }
    }

    fn getLambda(self: *SymbolTable, allocator: anytype) !isize {
        const lambda = try std.fmt.allocPrint(allocator, "lambda-{d}", .{self.idx});
        try self.table.put(lambda, self.idx);
        try self.reverse.put(self.idx, lambda);
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

    fn put(self: *Scope, key: isize, value: Expression) !void {
        try self.store.put(key, value);
    }
};

const TokenIterator = struct {
    buffer: [MAXWIDTH]u8,
    wordIterator: ?std.mem.TokenIterator(u8, std.mem.DelimiterType.scalar),
    word: ?[]const u8,

    fn next(self: *TokenIterator, stdin: anytype) !?[]const u8 {
        while (true) {
            if (self.wordIterator == null) {
                const line = if (try stdin.readUntilDelimiterOrEof(&self.buffer, '\n')) |nextLine| nextLine else return null;
                self.wordIterator = std.mem.tokenizeScalar(u8, line, ' ');
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
            const left = std.mem.indexOfScalar(u8, word, '(') orelse word.len;
            const right = std.mem.indexOfScalar(u8, word, ')') orelse word.len;
            const quote = std.mem.indexOfScalar(u8, word, '\'') orelse word.len;
            const idx = @max(@min(left, right, quote), 1);

            self.word = word[idx..];
            return word[0..idx];
        }
    }
};

const ExpressionIterator = struct {
    tokenIterator: TokenIterator,
    symbols: *SymbolTable,

    fn next(self: *ExpressionIterator, allocator: anytype, stdin: anytype) !?Expression {
        const token = if (try self.tokenIterator.next(stdin)) |nextToken| nextToken else return null;

        if (token[0] == '(') {
            var arrayList = std.ArrayList(Cons).init(allocator);
            while (try self.next(allocator, stdin)) |subexpression| {
                if (subexpression == .end) {
                    break;
                }
                try arrayList.append(Cons.singleton(subexpression));
            }
            if (arrayList.items.len > 0) {
                for (1.., arrayList.items[1..]) |idx, *item| {
                    arrayList.items[idx - 1].cdr = Expression.cons(item);
                }
                return Expression.cons(&arrayList.items[0]);
            }
            return Expression.nil();
        }

        if (token[0] == ')') {
            return Expression.end();
        }

        if (token[0] == '\'') {
            const subexpression = if (try self.next(allocator, stdin)) |nextSubexpression| nextSubexpression else return null;
            const quote = try allocator.alloc(Cons, 2);
            quote[0] = Cons.pair(Expression.word(try self.symbols.get("quote")), Expression.cons(&quote[1]));
            quote[1] = Cons.singleton(subexpression);
            return Expression.cons(&quote[0]);
        }

        const number = std.fmt.parseInt(isize, token, 10) catch {
            const word = try allocator.alloc(u8, token.len);
            std.mem.copyForwards(u8, word, token);
            return Expression.word(try self.symbols.get(word));
        };
        return Expression.number(number);
    }
};

fn eval(allocator: anytype, symbols: *SymbolTable, scope_: *Scope, expression_: Expression, final_: bool) !Expression {
    var scope = scope_;
    var expression = expression_;
    var final = final_;
    var free = false;
    const ret = ret: while (true) {
        switch (expression) {
            .number, .nil => break :ret expression,
            .word => |word| if (scope.get(word)) |variable| break :ret variable else return error.UnboundVariable,
            .cons => {},
            else => return error.InvalidSyntax,
        }
        const operation = try eval(allocator, symbols, scope, expression.cons.car, false);
        var args = expression.cons.cdr;
        switch (operation) {
            .function => |function| {
                var buffer: [MAXARGS]Expression = undefined;
                var fargs = function.args;
                var idx: usize = 0;
                while (fargs != .nil) : (fargs = fargs.cons.cdr) {
                    buffer[idx] = try eval(allocator, symbols, scope, args.cons.car, false);
                    args = args.cons.cdr;
                    idx += 1;
                }
                if (!final or function.name != scope.function) {
                    const functionScope = try allocator.create(Scope);
                    functionScope.* = Scope.init(allocator, scope, function.name);
                    scope = functionScope;
                    final = true;
                    free = true;
                }
                fargs = function.args;
                idx = 0;
                while (fargs != .nil) : (fargs = fargs.cons.cdr) {
                    try scope.put(fargs.cons.car.word, buffer[idx]);
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
                        const arg = (try eval(allocator, symbols, scope, args.cons.car, false)).number;
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
                        const number = try eval(allocator, symbols, scope, args.cons.car, false);
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
                    const fst = try eval(allocator, symbols, scope, args.cons.car, false);
                    const snd = try eval(allocator, symbols, scope, args.cons.cdr.cons.car, final);
                    if (builtin == .quotient) {
                        break :ret Expression.number(@divTrunc(fst.number, snd.number));
                    } else {
                        break :ret Expression.number(@mod(fst.number, snd.number));
                    }
                },
                .abs, .not => {
                    const arg = (try eval(allocator, symbols, scope, args.cons.car, final)).number;
                    if (builtin == .abs) {
                        break :ret Expression.number(if (arg < 0) -arg else arg);
                    } else {
                        break :ret Expression.number(if (arg == 0) 1 else 0);
                    }
                },
                .min, .max => {
                    var arg = (try eval(allocator, symbols, scope, args.cons.car, false)).number;
                    args = args.cons.cdr;
                    while (args != .nil) : (args = args.cons.cdr) {
                        const other = (try eval(allocator, symbols, scope, args.cons.car, false)).number;
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
                            const value = try eval(allocator, symbols, scope, args.cons.cdr.cons.car, false);
                            if (value == .empty) return error.InvalidSyntax;
                            try scope.put(word, value);
                        },
                        .cons => |definition| {
                            const function = try allocator.create(Function);
                            const body = try allocator.create(Cons);
                            body.* = Cons.pair(Expression.word(try symbols.get("begin")), args.cons.cdr);
                            function.* = Function.new(definition.cdr, Expression.cons(body), definition.car.word);
                            try scope.put(definition.car.word, Expression.function(function));
                        },
                        else => return error.InvalidSyntax,
                    }
                    break :ret Expression.empty();
                },
                .if_ => {
                    const condition = try eval(allocator, symbols, scope, args.cons.car, false);
                    if (condition.number == 0) {
                        expression = args.cons.cdr.cons.cdr.cons.car;
                    } else {
                        expression = args.cons.cdr.cons.car;
                    }
                    continue;
                },
                .cond => {
                    while (args != .nil) : (args = args.cons.cdr) {
                        if (args.cons.car.cons.car == .word and args.cons.car.cons.car.word == try symbols.get("else") or
                            (try eval(allocator, symbols, scope, args.cons.car.cons.car, false)).number != 0)
                        {
                            expression = args.cons.car.cons.cdr.cons.car;
                            continue :ret;
                        }
                    }
                    break :ret Expression.empty();
                },
                .eq => {
                    if (args == .nil) break :ret Expression.number(1);
                    const head = try eval(allocator, symbols, scope, args.cons.car, false);
                    args = args.cons.cdr;
                    while (args != .nil) : (args = args.cons.cdr) {
                        const number = try eval(allocator, symbols, scope, args.cons.car, false);
                        if (number.number != head.number) {
                            break :ret Expression.number(0);
                        }
                    }
                    break :ret Expression.number(1);
                },
                .lt, .le, .gt, .ge => {
                    const fst = try eval(allocator, symbols, scope, args.cons.car, false);
                    const snd = try eval(allocator, symbols, scope, args.cons.cdr.cons.car, final);
                    break :ret switch (builtin) {
                        .lt => Expression.number(if (fst.number < snd.number) 1 else 0),
                        .le => Expression.number(if (fst.number <= snd.number) 1 else 0),
                        .gt => Expression.number(if (fst.number > snd.number) 1 else 0),
                        else => Expression.number(if (fst.number >= snd.number) 1 else 0),
                    };
                },
                .begin => {
                    while (args != .nil) : (args = args.cons.cdr) {
                        if (args.cons.cdr == .nil) {
                            break :ret try eval(allocator, symbols, scope, args.cons.car, final);
                        } else {
                            _ = try eval(allocator, symbols, scope, args.cons.car, false);
                        }
                    }
                },
                .let, .letStar => {
                    var innerScope = Scope.init(allocator, scope, symbols.getScope());
                    defer innerScope.deinit();
                    var vars = args.cons.car;
                    args = args.cons.cdr;
                    while (vars != .nil) : (vars = vars.cons.cdr) {
                        try innerScope.put(vars.cons.car.cons.car.word, try eval(allocator, symbols, if (builtin == .let) scope else &innerScope, vars.cons.car.cons.cdr.cons.car, false));
                    }
                    while (args != .nil) : (args = args.cons.cdr) {
                        if (args.cons.cdr == .nil) {
                            break :ret try eval(allocator, symbols, &innerScope, args.cons.car, false);
                        } else {
                            _ = try eval(allocator, symbols, &innerScope, args.cons.car, false);
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
                    const list = try allocator.alloc(Cons, length);
                    for (0.., list) |idx, *item| {
                        if (idx < list.len - 1) {
                            item.* = Cons.pair(try eval(allocator, symbols, scope, args.cons.car, false), Expression.cons(&list[idx + 1]));
                        } else {
                            item.* = Cons.singleton(try eval(allocator, symbols, scope, args.cons.car, final));
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
                        break :ret try eval(allocator, symbols, scope, args.cons.car, final);
                    }
                    var arrayList = std.ArrayList(Cons).init(allocator);
                    while (args.cons.cdr != .nil) : (args = args.cons.cdr) {
                        var arg = try eval(allocator, symbols, scope, args.cons.car, false);
                        while (arg != .nil) : (arg = arg.cons.cdr) {
                            try arrayList.append(Cons.singleton(arg.cons.car));
                        }
                    }
                    const last = try eval(allocator, symbols, scope, args.cons.car, final);
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
                    var arg = try eval(allocator, symbols, scope, args.cons.cdr.cons.car, false);
                    if (arg == .nil) {
                        break :ret arg;
                    }
                    var argPtr = arg;
                    var length: usize = 0;
                    while (argPtr != .nil) : (argPtr = argPtr.cons.cdr) {
                        length += 1;
                    }
                    var list = try allocator.alloc(Cons, length);
                    for (0.., list) |idx, *item| {
                        var argument = Cons.singleton(arg.cons.car);
                        var call = Cons.pair(args.cons.car, Expression.cons(&argument));
                        if (idx < list.len - 1) {
                            item.* = Cons.pair(try eval(allocator, symbols, scope, Expression.cons(&call), false), Expression.cons(&list[idx + 1]));
                        } else {
                            item.* = Cons.pair(try eval(allocator, symbols, scope, Expression.cons(&call), final), Expression.nil());
                        }
                        arg = arg.cons.cdr;
                    }
                    break :ret Expression.cons(&list[0]);
                },
                .filter => {
                    var arg = try eval(allocator, symbols, scope, args.cons.cdr.cons.car, false);
                    if (arg == .nil) {
                        break :ret arg;
                    }
                    var arrayList = std.ArrayList(Cons).init(allocator);
                    while (arg != .nil) : (arg = arg.cons.cdr) {
                        var argument = Cons.singleton(arg.cons.car);
                        var call = Cons.pair(args.cons.car, Expression.cons(&argument));
                        const pred = try eval(allocator, symbols, scope, Expression.cons(&call), if (arg.cons.cdr != .nil) false else final);
                        if (pred.number != 0) {
                            try arrayList.append(argument);
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
                    const fst = try eval(allocator, symbols, scope, args.cons.car, false);
                    const snd = try eval(allocator, symbols, scope, args.cons.cdr.cons.car, final);
                    const pair = try allocator.create(Cons);
                    pair.* = Cons.pair(fst, snd);
                    break :ret Expression.cons(pair);
                },
                .car => break :ret (try eval(allocator, symbols, scope, args.cons.car, final)).cons.car,
                .cdr => break :ret (try eval(allocator, symbols, scope, args.cons.car, final)).cons.cdr,
                .cadr => break :ret (try eval(allocator, symbols, scope, args.cons.car, final)).cons.cdr.cons.car,
                .cddr => break :ret (try eval(allocator, symbols, scope, args.cons.car, final)).cons.cdr.cons.cdr,
                .length => {
                    var list = try eval(allocator, symbols, scope, args.cons.car, final);
                    var length: isize = 0;
                    while (list != .nil) : (list = list.cons.cdr) {
                        length += 1;
                    }
                    break :ret Expression.number(length);
                },
                .null_ => break :ret Expression{
                    .number = if (try eval(allocator, symbols, scope, args.cons.car, final) == .nil) 1 else 0,
                },
                .lambda => {
                    const function = try allocator.create(Function);
                    function.* = Function.new(args.cons.car, args.cons.cdr.cons.car, try symbols.getLambda(allocator));
                    break :ret Expression.function(function);
                },
            },
            else => return error.InvalidSyntax,
        }
        return error.InvalidSyntax;
    };
    if (free) {
        const parent = scope.parent.?;
        scope.deinit();
        allocator.destroy(scope);
        scope = parent;
    }
    return ret;
}

fn print(stdout: anytype, expression: Expression, symbols: SymbolTable) !void {
    switch (expression) {
        .number => |number| try stdout.print("{d}", .{number}),
        .word => |word| try stdout.print("{s}", .{symbols.str(word)}),
        .nil => try stdout.writeAll("()"),
        .cons => {
            try stdout.writeByte('(');
            var list = expression;
            while (list == .cons) : (list = list.cons.cdr) {
                try print(stdout, list.cons.car, symbols);
                if (list.cons.cdr != .nil) {
                    try stdout.writeByte(' ');
                }
            }
            if (list != .nil) {
                try stdout.writeAll(". ");
                try print(stdout, list, symbols);
            }
            try stdout.writeByte(')');
        },
        .builtin => |builtin| try stdout.print("#<procedure {s}>", .{builtin.toString()}),
        .function => |function| try stdout.print("#<procedure {s}>", .{symbols.str(function.name)}),
        .empty, .end => return error.InvalidSyntax,
    }
}

pub fn main() !void {
    const allocator = std.heap.c_allocator;
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    var symbols = SymbolTable.init(allocator);

    var scope = Scope.init(allocator, null, GLOBAL_SCOPE);
    for (0..@typeInfo(Builtin).Enum.fields.len) |idx| {
        const builtin: Builtin = @enumFromInt(idx);
        const id = try symbols.get(builtin.toString());
        try scope.put(id, Expression.builtin(builtin));
    }
    try scope.put(try symbols.get("#f"), Expression.number(0));
    try scope.put(try symbols.get("#t"), Expression.number(1));

    try stdout.writeAll("vitaly-la/scheme.zig version 1.0.0\n\n> ");

    const tokenIterator = TokenIterator{
        .buffer = undefined,
        .wordIterator = null,
        .word = null,
    };

    var expressionIterator = ExpressionIterator{ .tokenIterator = tokenIterator, .symbols = &symbols };
    while (try expressionIterator.next(allocator, stdin)) |expression| {
        const value = eval(allocator, &symbols, &scope, expression, false) catch |err| {
            std.debug.print("{}\n", .{err});
            try stdout.writeAll("> ");
            continue;
        };
        if (value == .empty) {
            try stdout.writeAll("> ");
            continue;
        }
        try print(stdout, value, symbols);
        try stdout.writeAll("\n> ");
    }

    try stdout.writeAll("\n");
}
