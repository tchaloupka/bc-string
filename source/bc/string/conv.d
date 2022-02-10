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
    static if (isSigned!T) {
         ptrdiff_t res;
        bool sign;
        if (str[0] == '-') {
            sign = true;
            count++;
        } else if (str[0] == '+')
            count++;
    } else size_t res;

    for (; count < str.length; ++count)
    {
        if (_expect(!str[count].isDigit, false)) return ParseResult!T.init;
        res = res*10 + (str[count]-'0');
    }

    if (_expect(res > T.max, false)) return ParseResult!T.init;

    static if (isSigned!T) {
        if (sign) res = -res;
    }
    return ParseResult!T(cast(T)res, count);
}

@safe unittest
{
    assert("42".parse!int == ParseResult!int(42, 2));
    assert("42".parse!uint == ParseResult!uint(42, 2));
    assert("-42".parse!int == ParseResult!int(-42, 3));
    assert("+42".parse!int == ParseResult!int(42, 3));
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
