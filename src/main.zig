const std = @import("std");

const MAXWIDTH = 128;

const BUILTINS = [_][]const u8{
    "+",
    "-",
    "*",
    "quotient",
    "modulo",
    "expt",
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
    "zero?",
    "eq?",
    "equal?",
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
    plus,
    minus,
    product,
    quotient,
    modulo,
    expt,
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
    zero,
    eq_,
    equal,
    lambda,

    fn toString(self: Builtin) []const u8 {
        return BUILTINS[@intFromEnum(self)];
    }
};

const Function = struct {
    args: Expression,
    body: Expression,
    closure: *Env,

    fn new(args: Expression, body: Expression, closure: *Env) Function {
        return Function{ .args = args, .body = body, .closure = closure };
    }
};

const Expression = union(enum) {
    number: isize,
    boolean: bool,
    symbol: usize,
    nil,
    cons: *Cons,
    builtin: Builtin,
    function: *const Function,
    empty,
    end,

    fn number(arg: isize) Expression {
        return Expression{ .number = arg };
    }

    fn boolean(arg: bool) Expression {
        return Expression{ .boolean = arg };
    }

    fn symbol(arg: usize) Expression {
        return Expression{ .symbol = arg };
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

    fn eql(self: Expression, rhs: Expression) bool {
        if (self == .cons and rhs == .cons) {
            return self.cons.car.eql(rhs.cons.car) and self.cons.cdr.eql(rhs.cons.cdr);
        } else {
            return std.meta.eql(self, rhs);
        }
    }
};

const SymbolTable = struct {
    table: std.StringHashMap(usize),
    reverse: std.AutoHashMap(usize, []const u8),
    idx: usize,

    fn init(allocator: anytype) SymbolTable {
        return SymbolTable{
            .table = std.StringHashMap(usize).init(allocator),
            .reverse = std.AutoHashMap(usize, []const u8).init(allocator),
            .idx = 0,
        };
    }

    fn get(self: *SymbolTable, key: []const u8) !usize {
        return self.table.get(key) orelse {
            try self.table.put(key, self.idx);
            try self.reverse.put(self.idx, key);
            self.idx += 1;
            return self.idx - 1;
        };
    }

    fn str(self: SymbolTable, key: usize) []const u8 {
        return self.reverse.get(key).?;
    }
};

const Env = struct {
    store: std.AutoHashMap(usize, Expression),
    parent: ?*Env,
    function: ?*const Function,

    fn init(allocator: anytype, parent: ?*Env, function: ?*const Function) Env {
        return Env{
            .store = std.AutoHashMap(usize, Expression).init(allocator),
            .parent = parent,
            .function = function,
        };
    }

    fn get(self: Env, key: usize) ?Expression {
        return self.store.get(key) orelse if (self.parent) |parent| parent.get(key) else null;
    }

    fn put(self: *Env, key: usize, value: Expression) !void {
        try self.store.put(key, value);
    }
};

const TokenIterator = struct {
    buffer: [MAXWIDTH]u8,
    symbolIterator: ?std.mem.TokenIterator(u8, std.mem.DelimiterType.scalar),
    symbol: ?[]const u8,

    fn next(self: *TokenIterator, stdin: anytype) !?[]const u8 {
        while (true) {
            if (self.symbolIterator == null) {
                const line = try stdin.readUntilDelimiterOrEof(&self.buffer, '\n') orelse return null;
                self.symbolIterator = std.mem.tokenizeScalar(u8, line, ' ');
            }

            if (self.symbol == null or self.symbol.?.len == 0) {
                self.symbol = self.symbolIterator.?.next();
                if (self.symbol == null or self.symbol.?[0] == ';') {
                    self.symbolIterator = null;
                    self.symbol = null;
                    continue;
                }
            }

            const symbol = self.symbol.?;
            const left = std.mem.indexOfScalar(u8, symbol, '(') orelse symbol.len;
            const right = std.mem.indexOfScalar(u8, symbol, ')') orelse symbol.len;
            const quote = std.mem.indexOfScalar(u8, symbol, '\'') orelse symbol.len;
            const idx = @max(@min(left, right, quote), 1);

            self.symbol = symbol[idx..];
            return symbol[0..idx];
        }
    }
};

const ExpressionIterator = struct {
    tokenIterator: TokenIterator,
    symbols: *SymbolTable,

    fn next(self: *ExpressionIterator, allocator: anytype, stdin: anytype) !?Expression {
        const token = try self.tokenIterator.next(stdin) orelse return null;

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
            const subexpression = try self.next(allocator, stdin) orelse return null;
            const quote = try allocator.alloc(Cons, 2);
            quote[0] = Cons.pair(Expression.symbol(try self.symbols.get("quote")), Expression.cons(&quote[1]));
            quote[1] = Cons.singleton(subexpression);
            return Expression.cons(&quote[0]);
        }

        const number = std.fmt.parseInt(isize, token, 10) catch {
            const symbol = try allocator.alloc(u8, token.len);
            std.mem.copyForwards(u8, symbol, token);
            const id = try self.symbols.get(symbol);
            if (id == try self.symbols.get("#f")) {
                return Expression.boolean(false);
            } else if (id == try self.symbols.get("#t")) {
                return Expression.boolean(true);
            } else if (id == try self.symbols.get("nil")) {
                return Expression.nil();
            } else {
                return Expression.symbol(id);
            }
        };
        return Expression.number(number);
    }
};

fn eval(allocator: anytype, symbols: *SymbolTable, env_: *Env, expression_: Expression) !Expression {
    var env = env_;
    var expression = expression_;
    var final = false;
    outer: while (true) {
        switch (expression) {
            .number, .boolean, .nil => return expression,
            .symbol => |symbol| return env.get(symbol) orelse return error.UnboundVariable,
            .cons => {},
            else => return error.InvalidSyntax,
        }
        const operation = try eval(allocator, symbols, env, expression.cons.car);
        var args = expression.cons.cdr;
        switch (operation) {
            .function => |function| {
                var length: usize = 0;
                var fargs = function.args;
                while (fargs != .nil) : (fargs = fargs.cons.cdr) {
                    length += 1;
                }
                var buffer = try allocator.alloc(Expression, length);
                defer allocator.free(buffer);
                var idx: usize = 0;
                fargs = function.args;
                while (fargs != .nil) : (fargs = fargs.cons.cdr) {
                    buffer[idx] = try eval(allocator, symbols, env, args.cons.car);
                    args = args.cons.cdr;
                    idx += 1;
                }
                if (!final or function != env.function) {
                    const functionEnv = try allocator.create(Env);
                    functionEnv.* = Env.init(allocator, function.closure, function);
                    env = functionEnv;
                    final = true;
                }
                idx = 0;
                fargs = function.args;
                while (fargs != .nil) : (fargs = fargs.cons.cdr) {
                    try env.put(fargs.cons.car.symbol, buffer[idx]);
                    idx += 1;
                }
                var body = function.body;
                while (body.cons.cdr != .nil) : (body = body.cons.cdr) {
                    _ = try eval(allocator, symbols, env, body.cons.car);
                }
                expression = body.cons.car;
                continue :outer;
            },
            .builtin => |builtin| switch (builtin) {
                .plus, .product => {
                    var result: isize = if (builtin == .plus) 0 else 1;
                    while (args != .nil) : (args = args.cons.cdr) {
                        const arg = try eval(allocator, symbols, env, args.cons.car);
                        if (builtin == .plus) {
                            result += arg.number;
                        } else {
                            result *= arg.number;
                        }
                    }
                    return Expression.number(result);
                },
                .and_, .or_ => {
                    while (args != .nil) : (args = args.cons.cdr) {
                        const arg = try eval(allocator, symbols, env, args.cons.car);
                        if (builtin == .and_ and arg.boolean == false) {
                            return Expression.boolean(false);
                        } else if (builtin == .or_ and arg.boolean == true) {
                            return Expression.boolean(true);
                        }
                    }
                    return Expression.boolean(if (builtin == .and_) true else false);
                },
                .minus => {
                    var result: isize = 0;
                    var fst = true;
                    while (args != .nil) : (args = args.cons.cdr) {
                        const number = try eval(allocator, symbols, env, args.cons.car);
                        if (fst and args.cons.cdr != .nil) {
                            result += number.number;
                        } else {
                            result -= number.number;
                        }
                        fst = false;
                    }
                    return Expression.number(result);
                },
                .quotient, .modulo, .expt => {
                    const fst = try eval(allocator, symbols, env, args.cons.car);
                    const snd = try eval(allocator, symbols, env, args.cons.cdr.cons.car);
                    switch (builtin) {
                        .quotient => return Expression.number(@divTrunc(fst.number, snd.number)),
                        .modulo => return Expression.number(@mod(fst.number, snd.number)),
                        else => return Expression.number(try std.math.powi(isize, fst.number, snd.number)),
                    }
                },
                .abs => {
                    const arg = try eval(allocator, symbols, env, args.cons.car);
                    return Expression.number(@max(arg.number, -arg.number));
                },
                .not => {
                    const arg = try eval(allocator, symbols, env, args.cons.car);
                    return Expression.boolean(!arg.boolean);
                },
                .min, .max => {
                    var arg = (try eval(allocator, symbols, env, args.cons.car)).number;
                    args = args.cons.cdr;
                    while (args != .nil) : (args = args.cons.cdr) {
                        const other = try eval(allocator, symbols, env, args.cons.car);
                        if (builtin == .min) {
                            arg = @min(arg, other.number);
                        } else {
                            arg = @max(arg, other.number);
                        }
                    }
                    return Expression.number(arg);
                },
                .quote => return args.cons.car,
                .define => {
                    switch (args.cons.car) {
                        .symbol => |symbol| {
                            const value = try eval(allocator, symbols, env, args.cons.cdr.cons.car);
                            if (value == .empty) return error.InvalidSyntax;
                            try env.put(symbol, value);
                            return Expression.symbol(symbol);
                        },
                        .cons => |definition| {
                            const function = try allocator.create(Function);
                            function.* = Function.new(definition.cdr, args.cons.cdr, env);
                            try env.put(definition.car.symbol, Expression.function(function));
                            return Expression.symbol(definition.car.symbol);
                        },
                        else => return error.InvalidSyntax,
                    }
                },
                .if_ => {
                    const condition = try eval(allocator, symbols, env, args.cons.car);
                    if (condition.boolean) {
                        expression = args.cons.cdr.cons.car;
                    } else if (args.cons.cdr.cons.cdr != .nil) {
                        expression = args.cons.cdr.cons.cdr.cons.car;
                    } else {
                        return Expression.empty();
                    }
                    continue :outer;
                },
                .cond => {
                    while (args != .nil) : (args = args.cons.cdr) {
                        if (args.cons.car.cons.car == .symbol and args.cons.car.cons.car.symbol == try symbols.get("else") or
                            (try eval(allocator, symbols, env, args.cons.car.cons.car)).boolean)
                        {
                            expression = args.cons.car.cons.cdr.cons.car;
                            continue :outer;
                        }
                    }
                    return Expression.empty();
                },
                .eq => {
                    if (args == .nil) return Expression.boolean(true);
                    const head = try eval(allocator, symbols, env, args.cons.car);
                    args = args.cons.cdr;
                    while (args != .nil) : (args = args.cons.cdr) {
                        const number = try eval(allocator, symbols, env, args.cons.car);
                        if (number.number != head.number) {
                            return Expression.boolean(false);
                        }
                    }
                    return Expression.boolean(true);
                },
                .lt, .le, .gt, .ge => {
                    const fst = try eval(allocator, symbols, env, args.cons.car);
                    const snd = try eval(allocator, symbols, env, args.cons.cdr.cons.car);
                    return switch (builtin) {
                        .lt => Expression.boolean(fst.number < snd.number),
                        .le => Expression.boolean(fst.number <= snd.number),
                        .gt => Expression.boolean(fst.number > snd.number),
                        else => Expression.boolean(fst.number >= snd.number),
                    };
                },
                .begin => {
                    while (args != .nil) : (args = args.cons.cdr) {
                        if (args.cons.cdr == .nil) {
                            expression = args.cons.car;
                            continue :outer;
                        } else {
                            _ = try eval(allocator, symbols, env, args.cons.car);
                        }
                    }
                },
                .let, .letStar => {
                    const innerEnv = try allocator.create(Env);
                    innerEnv.* = Env.init(allocator, env, null);
                    var vars = args.cons.car;
                    while (vars != .nil) : (vars = vars.cons.cdr) {
                        const value = try eval(allocator, symbols, if (builtin == .let) env else innerEnv, vars.cons.car.cons.cdr.cons.car);
                        try innerEnv.put(vars.cons.car.cons.car.symbol, value);
                    }
                    env = innerEnv;
                    final = true;
                    args = args.cons.cdr;
                    while (args.cons.cdr != .nil) : (args = args.cons.cdr) {
                        _ = try eval(allocator, symbols, env, args.cons.car);
                    }
                    expression = args.cons.car;
                    continue :outer;
                },
                .list => {
                    if (args == .nil) {
                        return args;
                    }
                    var argsPtr = args;
                    var length: usize = 0;
                    while (argsPtr != .nil) : (argsPtr = argsPtr.cons.cdr) {
                        length += 1;
                    }
                    const list = try allocator.alloc(Cons, length);
                    for (0.., list) |idx, *item| {
                        if (idx < list.len - 1) {
                            item.* = Cons.pair(try eval(allocator, symbols, env, args.cons.car), Expression.cons(&list[idx + 1]));
                        } else {
                            item.* = Cons.singleton(try eval(allocator, symbols, env, args.cons.car));
                        }
                        args = args.cons.cdr;
                    }
                    return Expression.cons(&list[0]);
                },
                .append => {
                    if (args == .nil) {
                        return args;
                    }
                    if (args.cons.cdr == .nil) {
                        return try eval(allocator, symbols, env, args.cons.car);
                    }
                    var arrayList = std.ArrayList(Cons).init(allocator);
                    while (args.cons.cdr != .nil) : (args = args.cons.cdr) {
                        var arg = try eval(allocator, symbols, env, args.cons.car);
                        while (arg != .nil) : (arg = arg.cons.cdr) {
                            try arrayList.append(Cons.singleton(arg.cons.car));
                        }
                    }
                    const last = try eval(allocator, symbols, env, args.cons.car);
                    if (arrayList.items.len == 0) {
                        arrayList.deinit();
                        return last;
                    }
                    for (1.., arrayList.items[1..]) |idx, *item| {
                        arrayList.items[idx - 1].cdr = Expression.cons(item);
                    }
                    arrayList.items[arrayList.items.len - 1].cdr = last;
                    return Expression.cons(&arrayList.items[0]);
                },
                .map => {
                    var arg = try eval(allocator, symbols, env, args.cons.cdr.cons.car);
                    if (arg == .nil) {
                        return arg;
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
                            item.* = Cons.pair(try eval(allocator, symbols, env, Expression.cons(&call)), Expression.cons(&list[idx + 1]));
                        } else {
                            item.* = Cons.pair(try eval(allocator, symbols, env, Expression.cons(&call)), Expression.nil());
                        }
                        arg = arg.cons.cdr;
                    }
                    return Expression.cons(&list[0]);
                },
                .filter => {
                    var arg = try eval(allocator, symbols, env, args.cons.cdr.cons.car);
                    if (arg == .nil) {
                        return arg;
                    }
                    var arrayList = std.ArrayList(Cons).init(allocator);
                    while (arg != .nil) : (arg = arg.cons.cdr) {
                        var argument = Cons.singleton(arg.cons.car);
                        var call = Cons.pair(args.cons.car, Expression.cons(&argument));
                        const pred = try eval(allocator, symbols, env, Expression.cons(&call));
                        if (pred.boolean) {
                            try arrayList.append(argument);
                        }
                    }
                    if (arrayList.items.len == 0) {
                        arrayList.deinit();
                        return Expression.nil();
                    }
                    for (1.., arrayList.items[1..]) |idx, *item| {
                        arrayList.items[idx - 1].cdr = Expression.cons(item);
                    }
                    return Expression.cons(&arrayList.items[0]);
                },
                .cons => {
                    const fst = try eval(allocator, symbols, env, args.cons.car);
                    const snd = try eval(allocator, symbols, env, args.cons.cdr.cons.car);
                    const pair = try allocator.create(Cons);
                    pair.* = Cons.pair(fst, snd);
                    return Expression.cons(pair);
                },
                .car => return (try eval(allocator, symbols, env, args.cons.car)).cons.car,
                .cdr => return (try eval(allocator, symbols, env, args.cons.car)).cons.cdr,
                .cadr => return (try eval(allocator, symbols, env, args.cons.car)).cons.cdr.cons.car,
                .cddr => return (try eval(allocator, symbols, env, args.cons.car)).cons.cdr.cons.cdr,
                .length => {
                    var list = try eval(allocator, symbols, env, args.cons.car);
                    var length: isize = 0;
                    while (list != .nil) : (list = list.cons.cdr) {
                        length += 1;
                    }
                    return Expression.number(length);
                },
                .null_ => return Expression.boolean(try eval(allocator, symbols, env, args.cons.car) == .nil),
                .zero => return Expression.boolean((try eval(allocator, symbols, env, args.cons.car)).number == 0),
                .eq_ => {
                    const fst = try eval(allocator, symbols, env, args.cons.car);
                    const snd = try eval(allocator, symbols, env, args.cons.cdr.cons.car);
                    return Expression.boolean(std.meta.eql(fst, snd));
                },
                .equal => {
                    const fst = try eval(allocator, symbols, env, args.cons.car);
                    const snd = try eval(allocator, symbols, env, args.cons.cdr.cons.car);
                    return Expression.boolean(fst.eql(snd));
                },
                .lambda => {
                    const function = try allocator.create(Function);
                    function.* = Function.new(args.cons.car, args.cons.cdr, env);
                    return Expression.function(function);
                },
            },
            else => return error.InvalidSyntax,
        }
        return error.InvalidSyntax;
    }
}

fn print(stdout: anytype, expression: Expression, symbols: SymbolTable) !void {
    switch (expression) {
        .number => |number| try stdout.print("{d}", .{number}),
        .boolean => |boolean| try stdout.print("{s}", .{if (boolean) "#t" else "#f"}),
        .symbol => |symbol| try stdout.print("{s}", .{symbols.str(symbol)}),
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
        .function => try stdout.print("#<procedure>", .{}),
        .empty, .end => return error.InvalidSyntax,
    }
}

pub fn main() !void {
    const allocator = std.heap.c_allocator;
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    var symbols = SymbolTable.init(allocator);

    var env = Env.init(allocator, null, null);
    for (0..@typeInfo(Builtin).Enum.fields.len) |idx| {
        const builtin: Builtin = @enumFromInt(idx);
        const id = try symbols.get(builtin.toString());
        try env.put(id, Expression.builtin(builtin));
    }

    try stdout.writeAll("vitaly-la/scheme.zig version 1.0.0\n\n> ");

    const tokenIterator = TokenIterator{
        .buffer = undefined,
        .symbolIterator = null,
        .symbol = null,
    };

    var expressionIterator = ExpressionIterator{ .tokenIterator = tokenIterator, .symbols = &symbols };
    while (try expressionIterator.next(allocator, stdin)) |expression| {
        const value = eval(allocator, &symbols, &env, expression) catch |err| {
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
