module bc.string.ascii;

/**
 * Simplified ASCII strings case insensitve comparison.
 * Returns:
 *   - < 0 - when first string is lesser that the second
 *   - = 0 - when both string are equal
 *   - > 0 - when first string is greater than the second
 */
int sicmp(const(char)[] a, const(char)[] b) @safe nothrow @nogc
{
    // special case for null
    if (a is null || b is null)
    {
        if (a is null && b !is null) return -1;
        if (a !is null && b is null) return 1;
        return 0;
    }

    immutable len = a.length > b.length ? b.length : a.length;
    version (Posix) {
        import core.sys.posix.strings : strncasecmp;
        immutable diff = () @trusted { return strncasecmp(a.ptr, b.ptr, len); }();
        if (diff) return diff;
    } else {
        // TODO: manual loop unroll
        for (int i=0; i < len; ++i) {
            auto lhs = a[i].toLower;
            auto rhs = b[i].toLower;
            auto diff = lhs - rhs;
            if (diff) return diff;
        }
    }
    return (a.length > b.length) - (b.length > a.length);
}

@safe unittest
{
    assert(sicmp("fOoo",  "FoOo")  == 0);
    assert(sicmp("abcd",  "efgh")  <  0);
    assert(sicmp("efgh",  "abcd")  >  0);
    assert(sicmp("fOoox", "FoOo")  >  0);
    assert(sicmp("fOoo",  "FoOox") <  0);
}

/// Converts ASCII characters 'A'..'Z' to a lower 'a'..'z'.
char toLower(char c) @safe pure nothrow @nogc
{
    pragma(inline, true);
    static immutable chmap = ()
        {
            char[256] res = void;

            for (int i=0; i < 256; ++i) {
                if (i >= 'A' && i <= 'Z') res[i] = cast(char)(i+32);
                else res[i] = cast(char)i;
            }
            return res;
        }();

    return chmap[c];
}

/// Converts ASCII characters 'a'..'z' to a upper 'A'..'Z'.
char toUpper(char c) @safe pure nothrow @nogc
{
    pragma(inline, true);
    static immutable chmap = ()
        {
            char[256] res = void;

            for (int i=0; i < 256; ++i) {
                if (i >= 'a' && i <= 'z') res[i] = cast(char)(i-32);
                else res[i] = cast(char)i;
            }
            return res;
        }();

    return chmap[c];
}

//@safe
unittest
{
    assert('A'.toLower == 'a');
    assert('b'.toLower == 'b');
    assert('2'.toLower == '2');
    assert('Z'.toLower == 'z');

    assert('a'.toUpper == 'A');
    assert('B'.toUpper == 'B');
    assert('2'.toUpper == '2');
    assert('z'.toUpper == 'Z');
}

/// Checks if character is a digit ('0'..'9')
bool isDigit(char c) @safe pure nothrow @nogc
{
    pragma(inline, true);
    static immutable chmap = ()
        {
            bool[256] res = void;

            for (int i=0; i < 256; ++i) {
                if (i >= '0' && i <= '9') res[i] = true;
                else res[i] = false;
            }
            return res;
        }();

    return chmap[c];
}

@safe unittest
{
    assert('0'.isDigit);
    assert('9'.isDigit);
    assert(!'a'.isDigit);
    assert(!'+'.isDigit);
}

/**
 * Compares two strings (case sensitive, locale independent).
 * Returns:
 *   - < 0 - when first string is lesser than the second
 *   - = 0 - when both strings are equal
 *   - > 0 - when first string is greater than the second
 */
int cmp(const(char)[] a, const(char)[] b) @safe pure nothrow @nogc
{
    import core.stdc.string : strncmp;

    // special case for null
    if (a is null || b is null)
    {
        if (a is null && b !is null) return -1;
        if (a !is null && b is null) return 1;
        return 0;
    }

    immutable len = a.length > b.length ? b.length : a.length;
    immutable r = () @trusted { return strncmp(a.ptr, b.ptr, len); }();
    if (r) return r;
    return (a.length > b.length) - (b.length > a.length);
}

@safe unittest
{
    assert(cmp(null, null) == 0);
    assert(cmp(null, "foo") < 0);
    assert(cmp("foo", null) > 0);
    assert(cmp("foo", "foo") == 0);
    assert(cmp("bar", "foo") < 0);
    assert(cmp("foo", "foobar") < 0);
}

/// Returns whether `c` is an ASCII whitespace character (space, tab, vertical
/// tab, form feed, carriage return, linefeed).
bool isWhite(dchar c) @safe pure nothrow @nogc
{
    return c == ' ' || (c >= 0x09 && c <= 0x0D);
}

@safe pure nothrow @nogc unittest
{
    assert( isWhite(' '));
    assert( isWhite('\t'));
    assert( isWhite('\n'));
    assert(!isWhite('1'));
    assert(!isWhite('a'));
}

/// Returns whether `c` is an ASCII letter or digit (0..9, a..z, A..Z).
bool isAlphaNum(dchar c) @safe pure nothrow @nogc
{
    return c <= 'z' && c >= '0' && (c <= '9' || c >= 'a' || (c >= 'A' && c <= 'Z'));
}

@safe pure nothrow @nogc unittest
{
    assert( isAlphaNum('A'));
    assert( isAlphaNum('1'));
    assert(!isAlphaNum('#'));
}

/// Checks if character is a hexadecimal digit ('0'..'9', 'a'..'f', 'A'..'F').
bool isHexDigit(char c) @safe pure nothrow @nogc
{
    pragma(inline, true);
    static immutable chmap = ()
        {
            bool[256] res = void;
            for (int i=0; i < 256; ++i)
                res[i] = (i >= '0' && i <= '9') || (i >= 'a' && i <= 'f') || (i >= 'A' && i <= 'F');
            return res;
        }();

    return chmap[c];
}

/// ditto
bool isHexDigit(dchar c) @safe pure nothrow @nogc
{
    return c <= 0xFF && isHexDigit(cast(char)c);
}

@safe unittest
{
    assert('0'.isHexDigit);
    assert('f'.isHexDigit);
    assert('F'.isHexDigit);
    assert(!'g'.isHexDigit);
    assert(!isHexDigit('ሴ'));
}

/// Converts a nibble (low 4 bits of `value`) to its upper-case hex character.
char toHexChar(uint value) @safe pure nothrow @nogc
{
    value &= 0x0F;
    return cast(char)((value < 10) ? ('0' + value) : ('A' + (value - 10)));
}

@safe unittest
{
    assert(toHexChar(0) == '0');
    assert(toHexChar(9) == '9');
    assert(toHexChar(10) == 'A');
    assert(toHexChar(15) == 'F');
}

/// CTFE alternative to `std.uni.toUpper` for ASCII string literals (usable in betterC).
template toUpper(string str)
{
    static if (str.length)
        enum toUpper = cast(char)((str[0] >= 'a' && str[0] <= 'z') ? (str[0] - ('a' - 'A')) : str[0])
            ~ toUpper!(str[1..$]);
    else
        enum toUpper = str[0..0];
}

/// CTFE alternative to `std.uni.toLower` for ASCII string literals (usable in betterC).
template toLower(string str)
{
    static if (str.length)
        enum toLower = cast(char)((str[0] >= 'A' && str[0] <= 'Z') ? (str[0] + ('a' - 'A')) : str[0])
            ~ toLower!(str[1..$]);
    else
        enum toLower = str[0..0];
}

unittest
{
    static assert(toUpper!"foo123" == "FOO123");
    static assert(toLower!"FOO123" == "foo123");
}
