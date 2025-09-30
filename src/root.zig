//! By convention, root.zig is the root source file when making a library.
const std = @import("std");

const str = []const u8;

const Token = union(enum) {
    illegal: str,
    eof: void,
    ident: str,
    int: str,
    assign: void,
    plus: void,
    comma: void,
    semicolon: void,
    lparen: void,
    rparen: void,
    lbrace: void,
    rbrace: void,
    function: void,
    let: void,
};

const keywords = std.StaticStringMap(Token).initComptime(.{
    .{ "let", .let },
    .{ "fn", .function },
});

const Index = usize;

const Lexer = struct {
    buffer: []const u8,
    pos: usize = 0,

    fn next(self: *Lexer) Token {
        if (self.pos >= self.buffer.len) {
            return .eof;
        }
        defer self.pos += 1;

        self.pos = findEndOfWhitespace(self.buffer, self.pos);

        const char = self.buffer[self.pos];

        return switch (char) {
            '=' => .assign,
            '+' => .plus,
            ',' => .comma,
            ';' => .semicolon,
            '(' => .lparen,
            ')' => .rparen,
            '{' => .lbrace,
            '}' => .rbrace,
            else => {
                if (isLetter(char)) {
                    const end = findEndOfIdentifier(self.buffer, self.pos);
                    const identifier = self.buffer[self.pos..end];
                    self.pos = end - 1; // minus one so that `self.pos` == `end` after defer statement

                    if (keywords.get(identifier)) |token| {
                        return token;
                    }

                    return .{ .ident = identifier };
                } else if (isNumber(char)) {
                    const end = findEndOfNumber(self.buffer, self.pos);
                    const number = self.buffer[self.pos..end];
                    self.pos = end - 1; // minus one so that `self.pos` == `end` after defer statement

                    return .{ .int = number };
                } else {
                    return .{ .illegal = self.buffer[self.pos .. self.pos + 1] };
                }
            },
        };
    }
};

fn findEndOfWhitespace(buffer: []const u8, start: usize) Index {
    var pos = start;
    var char = buffer[pos];

    while (char == ' ' or char == '\n' or char == '\r' or char == '\t') {
        pos += 1;
        char = buffer[pos];
    }

    return pos;
}

fn findEndOfIdentifier(buffer: []const u8, start: usize) usize {
    var end = start;

    while (isLetter(buffer[end]) and end <= buffer.len) {
        end += 1;
    }

    return end;
}

fn findEndOfNumber(buffer: []const u8, start: usize) usize {
    var end = start;

    while (isNumber(buffer[end]) and end <= buffer.len) {
        end += 1;
    }

    return end;
}
inline fn isLetter(char: u8) bool {
    return ('a' <= char and char <= 'z') or ('A' <= char and char <= 'Z') or char == '_';
}

inline fn isNumber(char: u8) bool {
    return '0' <= char and char <= '9' or char == '_';
}

test "next token" {
    const input =
        \\let five = 5;
        \\let ten = 10;
        \\
        \\let add = fn(x, y) {
        \\  x + y
        \\};
        \\
        \\ let result = add(five, ten);
    ;
    const test_cases = [_]Token{
        .let,
        .{ .ident = "five" },
        .assign,
        .{ .int = "5" },
        .semicolon,
        .let,
        .{ .ident = "ten" },
        .assign,
        .{ .int = "10" },
        .semicolon,
        .let,
        .{ .ident = "add" },
        .assign,
        .function,
        .lparen,
        .{ .ident = "x" },
        .comma,
        .{ .ident = "y" },
        .rparen,
        .lbrace,
        .{ .ident = "x" },
        .plus,
        .{ .ident = "y" },
        .rbrace,
        .semicolon,
        .let,
        .{ .ident = "result" },
        .assign,
        .{ .ident = "add" },
        .lparen,
        .{ .ident = "five" },
        .comma,
        .{ .ident = "ten" },
        .rparen,
        .semicolon,
        .eof,
    };

    var lexer = Lexer{ .buffer = input };

    for (0.., test_cases) |i, expected| {
        const actual = lexer.next();

        std.testing.expectEqual(@intFromEnum(expected), @intFromEnum(actual)) catch |err| {
            std.debug.print("Test Case {}: expected {t} token, got {t} at pos {}\n", .{ i, expected, actual, lexer.pos });
            switch (expected) {
                .ident, .int, .illegal => |literal| std.debug.print("Expected value \"{s}\" ", .{literal}),
                else => {},
            }
            switch (actual) {
                .ident, .int, .illegal => |literal| std.debug.print("Got unexpected value \"{s}\"\n", .{literal}),
                else => {},
            }
            return err;
        };

        switch (expected) {
            .ident => |literal| try std.testing.expectEqualSlices(u8, literal, actual.ident),
            .illegal => |literal| try std.testing.expectEqualSlices(u8, literal, actual.illegal),
            .int => |literal| try std.testing.expectEqualSlices(u8, literal, actual.int),
            else => {},
        }
    }
}

pub fn bufferedPrint() !void {
    // Stdout is for the actual output of your application, for example if you
    // are implementing gzip, then only the compressed bytes should be sent to
    // stdout, not any debugging messages.
    var stdout_buffer: [1024]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    try stdout.print("Run `zig build test` to run the tests.\n", .{});

    try stdout.flush(); // Don't forget to flush!
}

pub fn add(a: i32, b: i32) i32 {
    return a + b;
}

test "basic add functionality" {
    try std.testing.expect(add(3, 7) == 10);
}
