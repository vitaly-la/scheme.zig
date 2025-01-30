const std = @import("std");

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
    closure: *const Env,

    fn new(args: Expression, body: Expression, closure: *const Env) Function {
        return Function{ .args = args, .body = body, .closure = closure };
    }
};

const Expression = union(enum) {
    number: isize,
    boolean: bool,
    word: usize,
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

    fn word(arg: usize) Expression {
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
    closure: ?*const Env,
    parent: ?*Env,
    function: ?*const Function,

    fn init(allocator: anytype, closure: ?*const Env, parent: ?*Env, function: ?*const Function) Env {
        return Env{
            .store = std.AutoHashMap(usize, Expression).init(allocator),
            .closure = closure,
            .parent = parent,
            .function = function,
        };
    }

    fn get(self: Env, key: usize) ?Expression {
        return self.store.get(key) orelse (if (self.closure) |closure| closure.get(key) else null) orelse (if (self.parent) |parent| parent.get(key) else null);
    }

    fn put(self: *Env, key: usize, value: Expression) !void {
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
                const line = try stdin.readUntilDelimiterOrEof(&self.buffer, '\n') orelse return null;
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
            quote[0] = Cons.pair(Expression.word(try self.symbols.get("quote")), Expression.cons(&quote[1]));
            quote[1] = Cons.singleton(subexpression);
            return Expression.cons(&quote[0]);
        }

        const number = std.fmt.parseInt(isize, token, 10) catch {
            const word = try allocator.alloc(u8, token.len);
            std.mem.copyForwards(u8, word, token);
            const symbol = try self.symbols.get(word);
            if (symbol == try self.symbols.get("#f")) {
                return Expression.boolean(false);
            } else if (symbol == try self.symbols.get("#t")) {
                return Expression.boolean(true);
            } else if (symbol == try self.symbols.get("nil")) {
                return Expression.nil();
            } else {
                return Expression.word(symbol);
            }
        };
        return Expression.number(number);
    }
};

fn eval(allocator: anytype, symbols: *SymbolTable, env_: *Env, expression_: Expression, final_: bool) !Expression {
    var env = env_;
    var expression = expression_;
    var final = final_;
    var free = false;
    const ret = ret: while (true) {
        switch (expression) {
            .number, .boolean, .nil => break :ret expression,
            .word => |word| break :ret env.get(word) orelse return error.UnboundVariable,
            .cons => {},
            else => return error.InvalidSyntax,
        }
        const operation = try eval(allocator, symbols, env, expression.cons.car, false);
        var args = expression.cons.cdr;
        switch (operation) {
            .function => |function| {
                var buffer: [MAXARGS]Expression = undefined;
                var fargs = function.args;
                var idx: usize = 0;
                while (fargs != .nil) : (fargs = fargs.cons.cdr) {
                    buffer[idx] = try eval(allocator, symbols, env, args.cons.car, false);
                    args = args.cons.cdr;
                    idx += 1;
                }
                if (!final or function != env.function) {
                    const functionEnv = try allocator.create(Env);
                    functionEnv.* = Env.init(allocator, function.closure, env, function);
                    env = functionEnv;
                    final = true;
                    free = true;
                }
                fargs = function.args;
                idx = 0;
                while (fargs != .nil) : (fargs = fargs.cons.cdr) {
                    try env.put(fargs.cons.car.word, buffer[idx]);
                    idx += 1;
                }
                var body = function.body;
                while (body != .nil) : (body = body.cons.cdr) {
                    if (body.cons.cdr == .nil) {
                        expression = body.cons.car;
                        continue :ret;
                    } else {
                        _ = try eval(allocator, symbols, env, body.cons.car, false);
                    }
                }
            },
            .builtin => |builtin| switch (builtin) {
                .plus, .product => {
                    var result: isize = if (builtin == .plus) 0 else 1;
                    while (args != .nil) : (args = args.cons.cdr) {
                        const arg = try eval(allocator, symbols, env, args.cons.car, if (args.cons.cdr == .nil) final else false);
                        if (builtin == .plus) {
                            result += arg.number;
                        } else {
                            result *= arg.number;
                        }
                    }
                    break :ret Expression.number(result);
                },
                .and_, .or_ => {
                    while (args != .nil) : (args = args.cons.cdr) {
                        const arg = try eval(allocator, symbols, env, args.cons.car, if (args.cons.cdr == .nil) final else false);
                        if (builtin == .and_ and arg.boolean == false) {
                            break :ret Expression.boolean(false);
                        } else if (builtin == .or_ and arg.boolean == true) {
                            break :ret Expression.boolean(true);
                        }
                    }
                    break :ret Expression.boolean(if (builtin == .and_) true else false);
                },
                .minus => {
                    var result: isize = 0;
                    var fst = true;
                    while (args != .nil) : (args = args.cons.cdr) {
                        const number = try eval(allocator, symbols, env, args.cons.car, if (args.cons.cdr == .nil) final else false);
                        if (fst and args.cons.cdr != .nil) {
                            result += number.number;
                        } else {
                            result -= number.number;
                        }
                        fst = false;
                    }
                    break :ret Expression.number(result);
                },
                .quotient, .modulo, .expt => {
                    const fst = try eval(allocator, symbols, env, args.cons.car, false);
                    const snd = try eval(allocator, symbols, env, args.cons.cdr.cons.car, final);
                    switch (builtin) {
                        .quotient => break :ret Expression.number(@divTrunc(fst.number, snd.number)),
                        .modulo => break :ret Expression.number(@mod(fst.number, snd.number)),
                        else => break :ret Expression.number(try std.math.powi(isize, fst.number, snd.number)),
                    }
                },
                .abs => {
                    const arg = try eval(allocator, symbols, env, args.cons.car, final);
                    break :ret Expression.number(@max(arg.number, -arg.number));
                },
                .not => {
                    const arg = try eval(allocator, symbols, env, args.cons.car, final);
                    break :ret Expression.boolean(!arg.boolean);
                },
                .min, .max => {
                    var arg = (try eval(allocator, symbols, env, args.cons.car, false)).number;
                    args = args.cons.cdr;
                    while (args != .nil) : (args = args.cons.cdr) {
                        const other = try eval(allocator, symbols, env, args.cons.car, if (args.cons.cdr == .nil) final else false);
                        if (builtin == .min) {
                            arg = @min(arg, other.number);
                        } else {
                            arg = @max(arg, other.number);
                        }
                    }
                    break :ret Expression.number(arg);
                },
                .quote => break :ret args.cons.car,
                .define => {
                    switch (args.cons.car) {
                        .word => |word| {
                            const value = try eval(allocator, symbols, env, args.cons.cdr.cons.car, final);
                            if (value == .empty) return error.InvalidSyntax;
                            try env.put(word, value);
                            break :ret Expression.word(word);
                        },
                        .cons => |definition| {
                            const function = try allocator.create(Function);
                            function.* = Function.new(definition.cdr, args.cons.cdr, env);
                            try env.put(definition.car.word, Expression.function(function));
                            break :ret Expression.word(definition.car.word);
                        },
                        else => return error.InvalidSyntax,
                    }
                },
                .if_ => {
                    const condition = try eval(allocator, symbols, env, args.cons.car, false);
                    if (condition.boolean) {
                        expression = args.cons.cdr.cons.car;
                    } else if (args.cons.cdr.cons.cdr != .nil) {
                        expression = args.cons.cdr.cons.cdr.cons.car;
                    } else {
                        break :ret Expression.empty();
                    }
                    continue :ret;
                },
                .cond => {
                    while (args != .nil) : (args = args.cons.cdr) {
                        if (args.cons.car.cons.car == .word and args.cons.car.cons.car.word == try symbols.get("else") or
                            (try eval(allocator, symbols, env, args.cons.car.cons.car, false)).boolean)
                        {
                            expression = args.cons.car.cons.cdr.cons.car;
                            continue :ret;
                        }
                    }
                    break :ret Expression.empty();
                },
                .eq => {
                    if (args == .nil) break :ret Expression.boolean(true);
                    const head = try eval(allocator, symbols, env, args.cons.car, false);
                    args = args.cons.cdr;
                    while (args != .nil) : (args = args.cons.cdr) {
                        const number = try eval(allocator, symbols, env, args.cons.car, if (args.cons.cdr == .nil) final else false);
                        if (number.number != head.number) {
                            break :ret Expression.boolean(false);
                        }
                    }
                    break :ret Expression.boolean(true);
                },
                .lt, .le, .gt, .ge => {
                    const fst = try eval(allocator, symbols, env, args.cons.car, false);
                    const snd = try eval(allocator, symbols, env, args.cons.cdr.cons.car, final);
                    break :ret switch (builtin) {
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
                            continue :ret;
                        } else {
                            _ = try eval(allocator, symbols, env, args.cons.car, false);
                        }
                    }
                },
                .let, .letStar => {
                    const innerEnv = try allocator.create(Env);
                    innerEnv.* = Env.init(allocator, null, env, null);
                    var vars = args.cons.car;
                    while (vars != .nil) : (vars = vars.cons.cdr) {
                        const value = try eval(allocator, symbols, if (builtin == .let) env else innerEnv, vars.cons.car.cons.cdr.cons.car, false);
                        try innerEnv.put(vars.cons.car.cons.car.word, value);
                    }
                    env = innerEnv;
                    free = true;
                    args = args.cons.cdr;
                    while (args != .nil) : (args = args.cons.cdr) {
                        if (args.cons.cdr == .nil) {
                            expression = args.cons.car;
                            continue :ret;
                        } else {
                            _ = try eval(allocator, symbols, env, args.cons.car, false);
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
                            item.* = Cons.pair(try eval(allocator, symbols, env, args.cons.car, false), Expression.cons(&list[idx + 1]));
                        } else {
                            item.* = Cons.singleton(try eval(allocator, symbols, env, args.cons.car, final));
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
                        break :ret try eval(allocator, symbols, env, args.cons.car, final);
                    }
                    var arrayList = std.ArrayList(Cons).init(allocator);
                    while (args.cons.cdr != .nil) : (args = args.cons.cdr) {
                        var arg = try eval(allocator, symbols, env, args.cons.car, false);
                        while (arg != .nil) : (arg = arg.cons.cdr) {
                            try arrayList.append(Cons.singleton(arg.cons.car));
                        }
                    }
                    const last = try eval(allocator, symbols, env, args.cons.car, final);
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
                    var arg = try eval(allocator, symbols, env, args.cons.cdr.cons.car, false);
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
                            item.* = Cons.pair(try eval(allocator, symbols, env, Expression.cons(&call), false), Expression.cons(&list[idx + 1]));
                        } else {
                            item.* = Cons.pair(try eval(allocator, symbols, env, Expression.cons(&call), final), Expression.nil());
                        }
                        arg = arg.cons.cdr;
                    }
                    break :ret Expression.cons(&list[0]);
                },
                .filter => {
                    var arg = try eval(allocator, symbols, env, args.cons.cdr.cons.car, false);
                    if (arg == .nil) {
                        break :ret arg;
                    }
                    var arrayList = std.ArrayList(Cons).init(allocator);
                    while (arg != .nil) : (arg = arg.cons.cdr) {
                        var argument = Cons.singleton(arg.cons.car);
                        var call = Cons.pair(args.cons.car, Expression.cons(&argument));
                        const pred = try eval(allocator, symbols, env, Expression.cons(&call), if (arg.cons.cdr == .nil) final else false);
                        if (pred.boolean) {
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
                    const fst = try eval(allocator, symbols, env, args.cons.car, false);
                    const snd = try eval(allocator, symbols, env, args.cons.cdr.cons.car, final);
                    const pair = try allocator.create(Cons);
                    pair.* = Cons.pair(fst, snd);
                    break :ret Expression.cons(pair);
                },
                .car => break :ret (try eval(allocator, symbols, env, args.cons.car, final)).cons.car,
                .cdr => break :ret (try eval(allocator, symbols, env, args.cons.car, final)).cons.cdr,
                .cadr => break :ret (try eval(allocator, symbols, env, args.cons.car, final)).cons.cdr.cons.car,
                .cddr => break :ret (try eval(allocator, symbols, env, args.cons.car, final)).cons.cdr.cons.cdr,
                .length => {
                    var list = try eval(allocator, symbols, env, args.cons.car, final);
                    var length: isize = 0;
                    while (list != .nil) : (list = list.cons.cdr) {
                        length += 1;
                    }
                    break :ret Expression.number(length);
                },
                .null_ => break :ret Expression.boolean(try eval(allocator, symbols, env, args.cons.car, final) == .nil),
                .zero => break :ret Expression.boolean((try eval(allocator, symbols, env, args.cons.car, final)).number == 0),
                .eq_ => {
                    const fst = try eval(allocator, symbols, env, args.cons.car, false);
                    const snd = try eval(allocator, symbols, env, args.cons.cdr.cons.car, final);
                    break :ret Expression.boolean(std.meta.eql(fst, snd));
                },
                .equal => {
                    const fst = try eval(allocator, symbols, env, args.cons.car, false);
                    const snd = try eval(allocator, symbols, env, args.cons.cdr.cons.car, final);
                    break :ret Expression.boolean(fst.eql(snd));
                },
                .lambda => {
                    const function = try allocator.create(Function);
                    function.* = Function.new(args.cons.car, args.cons.cdr, env);
                    break :ret Expression.function(function);
                },
            },
            else => return error.InvalidSyntax,
        }
        return error.InvalidSyntax;
    };
    if (free) {
        env = env.parent.?;
    }
    return ret;
}

fn print(stdout: anytype, expression: Expression, symbols: SymbolTable) !void {
    switch (expression) {
        .number => |number| try stdout.print("{d}", .{number}),
        .boolean => |boolean| try stdout.print("{s}", .{if (boolean) "#t" else "#f"}),
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
        .function => try stdout.print("#<procedure>", .{}),
        .empty, .end => return error.InvalidSyntax,
    }
}

pub fn main() !void {
    const allocator = std.heap.c_allocator;
    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    var symbols = SymbolTable.init(allocator);

    var env = Env.init(allocator, null, null, null);
    for (0..@typeInfo(Builtin).Enum.fields.len) |idx| {
        const builtin: Builtin = @enumFromInt(idx);
        const id = try symbols.get(builtin.toString());
        try env.put(id, Expression.builtin(builtin));
    }

    try stdout.writeAll("vitaly-la/scheme.zig version 1.0.0\n\n> ");

    const tokenIterator = TokenIterator{
        .buffer = undefined,
        .wordIterator = null,
        .word = null,
    };

    var expressionIterator = ExpressionIterator{ .tokenIterator = tokenIterator, .symbols = &symbols };
    while (try expressionIterator.next(allocator, stdin)) |expression| {
        const value = eval(allocator, &symbols, &env, expression, false) catch |err| {
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
