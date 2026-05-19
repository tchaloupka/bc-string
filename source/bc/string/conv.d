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

ParseResult!T parse(T, ubyte radix = 10)(cstring str) if (isIntegral!T && !is(T == enum))
{
    pragma(inline, true);
    static assert(radix == 2 || radix == 8 || radix == 10 || radix == 16, "Unsupported radix");
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
        immutable c = str[count];
        size_t digit;
        static if (radix <= 10) {
            if (_expect(c < '0' || c > ('0' + radix - 1), false)) return ParseResult!T.init;
            digit = c - '0';
        } else {
            if (c >= '0' && c <= '9') digit = c - '0';
            else if (c >= 'a' && c <= 'f') digit = c - 'a' + 10;
            else if (c >= 'A' && c <= 'F') digit = c - 'A' + 10;
            else return ParseResult!T.init;
        }
        immutable next = res*radix + digit;
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

    assert("ff".parse!(int, 16) == ParseResult!int(0xff, 2));
    assert("1A".parse!(int, 16) == ParseResult!int(0x1a, 2));
    assert("101".parse!(int, 2) == ParseResult!int(5, 3));
    assert("17".parse!(int, 8) == ParseResult!int(15, 2));
    assert(!cast(bool)"2".parse!(int, 2)); // invalid digit for radix
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

/**
 * Checks correctness of a string for `hexString`: only whitespace characters and
 * an even number of hexadecimal digits (case insensitive) are allowed.
 */
bool isHexLiteral(String)(scope const String hexData) @safe pure nothrow @nogc
{
    size_t i;
    foreach (c; hexData) // iterate by code unit - no autodecoding
    {
        switch (c)
        {
            case ' ', '\t', '\v', '\f', '\r', '\n':
                continue;
            default:
                break;
        }
        if (c.isHexDigit) ++i;
        else return false;
    }
    return !(i & 1);
}

/**
 * Converts a hex literal to a string at compile time. Each pair of hexadecimal digits
 * becomes one code unit; whitespace in the input is ignored (it can be used to keep the
 * literal readable). A betterC-friendly equivalent of the `x"..."` hex string literal.
 */
template hexString(alias hexData)
if (hexData.isHexLiteral)
{
    alias STR = typeof(hexData);
    enum STR hexString = mixin(() @trusted
    {
        import std.traits : Unqual;
        import bc.internal.range : ElementEncodingType;

        alias C = Unqual!(ElementEncodingType!STR); // char, wchar or dchar
        C[] result;
        result.length = 1 + hexData.length * 2 + 1;
        auto r = result.ptr;
        r[0] = '"';
        size_t cnt = 0;
        foreach (c; hexData)
        {
            if (c.isHexDigit)
            {
                if ((cnt & 1) == 0)
                {
                    r[1 + cnt]     = '\\';
                    r[1 + cnt + 1] = 'x';
                    cnt += 2;
                }
                r[1 + cnt] = c;
                ++cnt;
            }
        }
        r[1 + cnt] = '"';
        result.length = 1 + cnt + 1;
        return result;
    }());
}

@safe unittest
{
    static assert(hexString!"4a 4b 4c" == "JKL");
    static assert(hexString!"deadbeef".length == 4);
    assert(!isHexLiteral("abc"));   // odd digit count
    assert(!isHexLiteral("xy"));    // non-hex
    assert(isHexLiteral("de ad"));
}
