/**
 * Parsing from string to standard types.
 */
module bc.string.conv;

import bc.string.ascii;
import bc.core.intrinsics;
import std.traits;

alias cstring = const(char)[];

//TODO: check for owerloads?
//TODO: use parseToken to fast check length of input number

ParseResult!T parse(T)(cstring str) if (isIntegral!T && !is(T == enum))
{
    pragma(inline, true);
    if (!str.length) return ParseResult!T.init;

    size_t count;
    size_t res; // accumulate magnitude unsigned so T.min and the full unsigned range fit
    bool sign;
    static if (isSigned!T) {
        if (str[0] == '-') {
            sign = true;
            count++;
        } else if (str[0] == '+')
            count++;
    }
    if (_expect(count >= str.length, false)) return ParseResult!T.init; // sign char only

    for (; count < str.length; ++count)
    {
        if (_expect(!str[count].isDigit, false)) return ParseResult!T.init;
        immutable next = res*10 + (str[count]-'0');
        if (_expect(next < res, false)) return ParseResult!T.init; // overflow
        res = next;
    }

    static if (isSigned!T) {
        if (sign) {
            if (_expect(res > cast(size_t)T.max + 1, false)) return ParseResult!T.init;
            return ParseResult!T(cast(T)(-res), count);
        }
    }
    if (_expect(res > T.max, false)) return ParseResult!T.init;
    return ParseResult!T(cast(T)res, count);
}

@safe unittest
{
    assert("42".parse!int == ParseResult!int(42, 2));
    assert("42".parse!uint == ParseResult!uint(42, 2));
    assert("-42".parse!int == ParseResult!int(-42, 3));
    assert("+42".parse!int == ParseResult!int(42, 3));
    assert("-2147483648".parse!int == ParseResult!int(int.min, 11));
    assert("-128".parse!byte == ParseResult!byte(byte.min, 4));
    assert("255".parse!ubyte == ParseResult!ubyte(ubyte.max, 3));
    assert(!cast(bool)"256".parse!ubyte); // overflow rejected
    assert(!cast(bool)"-".parse!int);     // lone sign rejected
}

/// Result of the parse functions
struct ParseResult(T)
{
    T data;       /// parsed value
    size_t count; /// Number of characters consumed while parsing (0 means that input couldn't be parsed into specified type)

    /// Checks if some data was actually parsed from the input
    bool opCast(T)() const if(is(T == bool)) { return count > 0; }
    // alias data this;
}
