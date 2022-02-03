module bc.string.ascii;

/**
 * Simplified ASCII strings case insensitve comparison.
 * Returns:
 *   - < 0 - when first string is lesser that the second
 *   - = 0 - when both string are equal
 *   - > 0 - when first string is greater than the second
 */
int sicmp(const(char)[] a, const(char)[] b) @safe nothrow @nogc
in (a.length && b.length, "empty string")
{
    immutable len = a.length > b.length ? b.length : a.length;
    version (Posix) {
        import core.sys.posix.strings : strncasecmp;
        immutable diff = () @trusted { return strncasecmp(a.ptr, b.ptr, len); }();
        if (diff) return diff;
    } else {
        // TODO: manual loop unroll
        immutable end = min(a.length, b.length);
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
