const std = @import("std");

const allocator = std.heap.c_allocator;

const MAXWIDTH = 128;
const MAXARGS = 16;

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
    "set!",
};

fn oom() noreturn {
    std.debug.panic("Out of memory", .{});
}

fn ioerr() noreturn {
    std.debug.panic("IO error", .{});
}

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
    set,

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
    cons: *const Cons,
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

    fn cons(arg: *const Cons) Expression {
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

    fn init() SymbolTable {
        return SymbolTable{
            .table = std.StringHashMap(usize).init(allocator),
            .reverse = std.AutoHashMap(usize, []const u8).init(allocator),
            .idx = 0,
        };
    }

    fn get(self: *SymbolTable, key: []const u8) usize {
        return self.table.get(key) orelse {
            self.table.put(key, self.idx) catch oom();
            self.reverse.put(self.idx, key) catch oom();
            self.idx += 1;
            return self.idx - 1;
        };
    }

    fn str(self: SymbolTable, key: usize) []const u8 {
        return self.reverse.get(key).?;
    }
};

const Point = struct { value: Expression, cached: bool };

const Item = struct { key: usize, value: Point };

const IntHashMap = struct {
    store: []?Item,
    mask: usize,
    count: usize,
    size: usize,

    fn init() IntHashMap {
        const store = allocator.alloc(?Item, 16) catch oom();
        @memset(store, null);
        return IntHashMap{ .store = store, .mask = 15, .count = 0, .size = 16 };
    }

    fn get(self: IntHashMap, key_: usize) ?Point {
        var key = key_;
        while (self.store[key & self.mask]) |item| : (key += 1) {
            if (item.key == key_) {
                return item.value;
            }
        }
        return null;
    }

    fn put(self: *IntHashMap, key_: usize, value: Point) void {
        var key = key_;
        while (self.store[key & self.mask]) |item| : (key += 1) {
            if (item.key == key_) {
                self.store[key & self.mask] = .{ .key = key_, .value = value };
                return;
            }
        }
        self.store[key & self.mask] = .{ .key = key_, .value = value };
        self.count += 1;
        if (100 * self.count > 80 * self.size) {
            self.mask = (self.mask << 1) | 1;
            self.size <<= 1;
            const newStore = allocator.alloc(?Item, self.size) catch oom();
            for (self.store) |item_| {
                if (item_) |item| {
                    var itemKey = item.key;
                    while (newStore[itemKey & self.mask]) |_| : (itemKey += 1) {}
                    newStore[itemKey & self.mask] = item;
                }
            }
            allocator.free(self.store);
            self.store = newStore;
        }
    }
};

const Env = struct {
    store: IntHashMap,
    parent: ?*Env,
    function: ?*const Function,

    fn init(parent: ?*Env, function: ?*const Function) Env {
        return Env{
            .store = IntHashMap.init(),
            .parent = parent,
            .function = function,
        };
    }

    fn get(self: Env, key: usize) !Expression {
        if (self.store.get(key)) |point| {
            return point.value;
        } else if (self.parent) |parent| {
            return parent.get(key);
        }
        return error.UnboundVariable;
    }

    fn getCaching(self: *Env, key: usize) !Expression {
        if (self.store.get(key)) |point| {
            return point.value;
        }
        if (self.parent) |parent| {
            const value = try parent.get(key);
            self.store.put(key, .{ .value = value, .cached = true });
            return value;
        }
        return error.UnboundVariable;
    }

    fn put(self: *Env, key: usize, value: Expression) void {
        self.store.put(key, .{ .value = value, .cached = false });
    }

    fn set(self: *Env, key: usize, value: Expression) void {
        if (self.store.get(key)) |point| {
            if (point.cached) {
                return self.parent.?.set(key, value);
            }
        } else if (self.parent) |parent| {
            if (parent.get(key) catch null) |_| {
                return parent.set(key, value);
            }
        }
        self.store.put(key, .{ .value = value, .cached = false });
    }
};

const TokenIterator = struct {
    buffer: [MAXWIDTH]u8,
    symbolIterator: ?std.mem.TokenIterator(u8, std.mem.DelimiterType.scalar),
    symbol: ?[]const u8,

    fn next(self: *TokenIterator, stdin: anytype) ?[]const u8 {
        while (true) {
            if (self.symbolIterator == null) {
                const line = stdin.readUntilDelimiterOrEof(&self.buffer, '\n') catch ioerr() orelse return null;
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

    fn next(self: *ExpressionIterator, stdin: anytype) ?Expression {
        const token = self.tokenIterator.next(stdin) orelse return null;

        if (token[0] == '(') {
            var arrayList = std.ArrayList(Cons).init(allocator);
            while (self.next(stdin)) |subexpression| {
                if (subexpression == .end) {
                    break;
                }
                arrayList.append(Cons.singleton(subexpression)) catch oom();
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
            const subexpression = self.next(stdin) orelse return null;
            const quote = allocator.alloc(Cons, 2) catch oom();
            quote[0] = Cons.pair(Expression.symbol(self.symbols.get("quote")), Expression.cons(&quote[1]));
            quote[1] = Cons.singleton(subexpression);
            return Expression.cons(&quote[0]);
        }

        const number = std.fmt.parseInt(isize, token, 10) catch {
            const symbol = allocator.alloc(u8, token.len) catch oom();
            std.mem.copyForwards(u8, symbol, token);
            const id = self.symbols.get(symbol);
            if (id == self.symbols.get("#f")) {
                return Expression.boolean(false);
            } else if (id == self.symbols.get("#t")) {
                return Expression.boolean(true);
            } else if (id == self.symbols.get("nil")) {
                return Expression.nil();
            } else {
                return Expression.symbol(id);
            }
        };
        return Expression.number(number);
    }
};

inline fn eval(symbols: *SymbolTable, env: *Env, expression: Expression) !Expression {
    switch (expression) {
        .number, .boolean, .nil => return expression,
        .symbol => |symbol| return env.getCaching(symbol),
        .cons => return evalCons(symbols, env, expression),
        else => return error.InvalidSyntax,
    }
}

fn evalCons(symbols: *SymbolTable, env_: *Env, expression_: Expression) error{ InvalidSyntax, Overflow, UnboundVariable, Underflow }!Expression {
    var env = env_;
    var expression = expression_;
    var final = false;
    outer: while (true) {
        switch (expression) {
            .number, .boolean, .nil => return expression,
            .symbol => |symbol| return env.getCaching(symbol),
            .cons => {},
            else => return error.InvalidSyntax,
        }
        const operation = try eval(symbols, env, expression.cons.car);
        var args = expression.cons.cdr;
        switch (operation) {
            .function => |function| {
                var buffer: [MAXARGS]Expression = undefined;
                var idx: usize = 0;
                var fargs = function.args;
                while (fargs != .nil) : (fargs = fargs.cons.cdr) {
                    buffer[idx] = try eval(symbols, env, args.cons.car);
                    args = args.cons.cdr;
                    idx += 1;
                }
                if (!final or function != env.function) {
                    env = allocator.create(Env) catch oom();
                    env.* = Env.init(function.closure, function);
                    final = true;
                }
                idx = 0;
                fargs = function.args;
                while (fargs != .nil) : (fargs = fargs.cons.cdr) {
                    env.put(fargs.cons.car.symbol, buffer[idx]);
                    idx += 1;
                }
                var body = function.body;
                while (body.cons.cdr != .nil) : (body = body.cons.cdr) {
                    _ = try eval(symbols, env, body.cons.car);
                }
                expression = body.cons.car;
                continue :outer;
            },
            .builtin => |builtin| switch (builtin) {
                .plus, .product => {
                    var result: isize = if (builtin == .plus) 0 else 1;
                    while (args != .nil) : (args = args.cons.cdr) {
                        const arg = try eval(symbols, env, args.cons.car);
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
                        if (args.cons.cdr == .nil) {
                            expression = args.cons.car;
                            continue :outer;
                        }
                        const arg = try eval(symbols, env, args.cons.car);
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
                        const number = try eval(symbols, env, args.cons.car);
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
                    const fst = try eval(symbols, env, args.cons.car);
                    const snd = try eval(symbols, env, args.cons.cdr.cons.car);
                    switch (builtin) {
                        .quotient => return Expression.number(@divTrunc(fst.number, snd.number)),
                        .modulo => return Expression.number(@mod(fst.number, snd.number)),
                        else => return Expression.number(try std.math.powi(isize, fst.number, snd.number)),
                    }
                },
                .abs => {
                    const arg = try eval(symbols, env, args.cons.car);
                    return Expression.number(@max(arg.number, -arg.number));
                },
                .not => {
                    const arg = try eval(symbols, env, args.cons.car);
                    return Expression.boolean(!arg.boolean);
                },
                .min, .max => {
                    var arg = (try eval(symbols, env, args.cons.car)).number;
                    args = args.cons.cdr;
                    while (args != .nil) : (args = args.cons.cdr) {
                        const other = try eval(symbols, env, args.cons.car);
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
                            const value = try eval(symbols, env, args.cons.cdr.cons.car);
                            if (value == .empty) return error.InvalidSyntax;
                            env.put(symbol, value);
                            return Expression.symbol(symbol);
                        },
                        .cons => |definition| {
                            const function = allocator.create(Function) catch oom();
                            function.* = Function.new(definition.cdr, args.cons.cdr, env);
                            env.put(definition.car.symbol, Expression.function(function));
                            return Expression.symbol(definition.car.symbol);
                        },
                        else => return error.InvalidSyntax,
                    }
                },
                .if_ => {
                    const condition = try eval(symbols, env, args.cons.car);
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
                        if (args.cons.car.cons.car == .symbol and args.cons.car.cons.car.symbol == symbols.get("else") or
                            (try eval(symbols, env, args.cons.car.cons.car)).boolean)
                        {
                            expression = args.cons.car.cons.cdr.cons.car;
                            continue :outer;
                        }
                    }
                    return Expression.empty();
                },
                .eq => {
                    if (args == .nil) return Expression.boolean(true);
                    const head = try eval(symbols, env, args.cons.car);
                    args = args.cons.cdr;
                    while (args != .nil) : (args = args.cons.cdr) {
                        const number = try eval(symbols, env, args.cons.car);
                        if (number.number != head.number) {
                            return Expression.boolean(false);
                        }
                    }
                    return Expression.boolean(true);
                },
                .lt, .le, .gt, .ge => {
                    const fst = try eval(symbols, env, args.cons.car);
                    const snd = try eval(symbols, env, args.cons.cdr.cons.car);
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
                            _ = try eval(symbols, env, args.cons.car);
                        }
                    }
                },
                .let, .letStar => {
                    const innerEnv = allocator.create(Env) catch oom();
                    innerEnv.* = Env.init(env, null);
                    var vars = args.cons.car;
                    while (vars != .nil) : (vars = vars.cons.cdr) {
                        const value = try eval(symbols, if (builtin == .let) env else innerEnv, vars.cons.car.cons.cdr.cons.car);
                        innerEnv.put(vars.cons.car.cons.car.symbol, value);
                    }
                    env = innerEnv;
                    args = args.cons.cdr;
                    while (args.cons.cdr != .nil) : (args = args.cons.cdr) {
                        _ = try eval(symbols, env, args.cons.car);
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
                    const list = allocator.alloc(Cons, length) catch oom();
                    for (0.., list) |idx, *item| {
                        if (idx < list.len - 1) {
                            item.* = Cons.pair(try eval(symbols, env, args.cons.car), Expression.cons(&list[idx + 1]));
                        } else {
                            item.* = Cons.singleton(try eval(symbols, env, args.cons.car));
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
                        expression = args.cons.car;
                        continue :outer;
                    }
                    var arrayList = std.ArrayList(Cons).init(allocator);
                    while (args.cons.cdr != .nil) : (args = args.cons.cdr) {
                        var arg = try eval(symbols, env, args.cons.car);
                        while (arg != .nil) : (arg = arg.cons.cdr) {
                            arrayList.append(Cons.singleton(arg.cons.car)) catch oom();
                        }
                    }
                    const last = try eval(symbols, env, args.cons.car);
                    if (arrayList.items.len == 0) {
                        return last;
                    }
                    for (1.., arrayList.items[1..]) |idx, *item| {
                        arrayList.items[idx - 1].cdr = Expression.cons(item);
                    }
                    arrayList.items[arrayList.items.len - 1].cdr = last;
                    return Expression.cons(&arrayList.items[0]);
                },
                .map => {
                    var arg = try eval(symbols, env, args.cons.cdr.cons.car);
                    if (arg == .nil) {
                        return arg;
                    }
                    var argPtr = arg;
                    var length: usize = 0;
                    while (argPtr != .nil) : (argPtr = argPtr.cons.cdr) {
                        length += 1;
                    }
                    var list = allocator.alloc(Cons, length) catch oom();
                    for (0.., list) |idx, *item| {
                        var argument = Cons.singleton(arg.cons.car);
                        var call = Cons.pair(args.cons.car, Expression.cons(&argument));
                        if (idx < list.len - 1) {
                            item.* = Cons.pair(try eval(symbols, env, Expression.cons(&call)), Expression.cons(&list[idx + 1]));
                        } else {
                            item.* = Cons.pair(try eval(symbols, env, Expression.cons(&call)), Expression.nil());
                        }
                        arg = arg.cons.cdr;
                    }
                    return Expression.cons(&list[0]);
                },
                .filter => {
                    var arg = try eval(symbols, env, args.cons.cdr.cons.car);
                    if (arg == .nil) {
                        return arg;
                    }
                    var arrayList = std.ArrayList(Cons).init(allocator);
                    while (arg != .nil) : (arg = arg.cons.cdr) {
                        var argument = Cons.singleton(arg.cons.car);
                        var call = Cons.pair(args.cons.car, Expression.cons(&argument));
                        const pred = try eval(symbols, env, Expression.cons(&call));
                        if (pred.boolean) {
                            arrayList.append(argument) catch oom();
                        }
                    }
                    if (arrayList.items.len == 0) {
                        return Expression.nil();
                    }
                    for (1.., arrayList.items[1..]) |idx, *item| {
                        arrayList.items[idx - 1].cdr = Expression.cons(item);
                    }
                    return Expression.cons(&arrayList.items[0]);
                },
                .cons => {
                    const fst = try eval(symbols, env, args.cons.car);
                    const snd = try eval(symbols, env, args.cons.cdr.cons.car);
                    const pair = allocator.create(Cons) catch oom();
                    pair.* = Cons.pair(fst, snd);
                    return Expression.cons(pair);
                },
                .car => return (try eval(symbols, env, args.cons.car)).cons.car,
                .cdr => return (try eval(symbols, env, args.cons.car)).cons.cdr,
                .cadr => return (try eval(symbols, env, args.cons.car)).cons.cdr.cons.car,
                .cddr => return (try eval(symbols, env, args.cons.car)).cons.cdr.cons.cdr,
                .length => {
                    var list = try eval(symbols, env, args.cons.car);
                    var length: isize = 0;
                    while (list != .nil) : (list = list.cons.cdr) {
                        length += 1;
                    }
                    return Expression.number(length);
                },
                .null_ => return Expression.boolean(try eval(symbols, env, args.cons.car) == .nil),
                .zero => return Expression.boolean((try eval(symbols, env, args.cons.car)).number == 0),
                .eq_ => {
                    const fst = try eval(symbols, env, args.cons.car);
                    const snd = try eval(symbols, env, args.cons.cdr.cons.car);
                    return Expression.boolean(std.meta.eql(fst, snd));
                },
                .equal => {
                    const fst = try eval(symbols, env, args.cons.car);
                    const snd = try eval(symbols, env, args.cons.cdr.cons.car);
                    return Expression.boolean(fst.eql(snd));
                },
                .lambda => {
                    const function = allocator.create(Function) catch oom();
                    function.* = Function.new(args.cons.car, args.cons.cdr, env);
                    return Expression.function(function);
                },
                .set => {
                    const value = try eval(symbols, env, args.cons.cdr.cons.car);
                    if (value == .empty) return error.InvalidSyntax;
                    env.set(args.cons.car.symbol, value);
                    return Expression.empty();
                },
            },
            else => return error.InvalidSyntax,
        }
        return error.InvalidSyntax;
    }
}

fn print(stdout: anytype, expression: Expression, symbols: SymbolTable) void {
    switch (expression) {
        .number => |number| stdout.print("{d}", .{number}) catch ioerr(),
        .boolean => |boolean| stdout.print("{s}", .{if (boolean) "#t" else "#f"}) catch ioerr(),
        .symbol => |symbol| stdout.print("{s}", .{symbols.str(symbol)}) catch ioerr(),
        .nil => stdout.writeAll("()") catch ioerr(),
        .cons => {
            stdout.writeByte('(') catch ioerr();
            var list = expression;
            while (list == .cons) : (list = list.cons.cdr) {
                print(stdout, list.cons.car, symbols);
                if (list.cons.cdr != .nil) {
                    stdout.writeByte(' ') catch ioerr();
                }
            }
            if (list != .nil) {
                stdout.writeAll(". ") catch ioerr();
                print(stdout, list, symbols);
            }
            stdout.writeByte(')') catch ioerr();
        },
        .builtin => |builtin| stdout.print("#<procedure {s}>", .{builtin.toString()}) catch ioerr(),
        .function => stdout.print("#<procedure>", .{}) catch ioerr(),
        .empty, .end => std.debug.panic("{} expression must not be printed", .{expression}),
    }
}

pub fn main() void {
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    var symbols = SymbolTable.init();

    var env = Env.init(null, null);
    for (0..@typeInfo(Builtin).Enum.fields.len) |idx| {
        const builtin: Builtin = @enumFromInt(idx);
        const id = symbols.get(builtin.toString());
        env.put(id, Expression.builtin(builtin));
    }

    stdout.writeAll("vitaly-la/scheme.zig version 1.0.0\n\n> ") catch ioerr();

    const tokenIterator = TokenIterator{
        .buffer = undefined,
        .symbolIterator = null,
        .symbol = null,
    };

    var expressionIterator = ExpressionIterator{ .tokenIterator = tokenIterator, .symbols = &symbols };
    while (expressionIterator.next(stdin)) |expression| {
        const value = eval(&symbols, &env, expression) catch |err| {
            std.debug.print("{}\n", .{err});
            stdout.writeAll("> ") catch ioerr();
            continue;
        };
        if (value == .empty) {
            stdout.writeAll("> ") catch ioerr();
            continue;
        }
        print(stdout, value, symbols);
        stdout.writeAll("\n> ") catch ioerr();
    }

    stdout.writeAll("\n") catch ioerr();
}
