module bc.string.numeric;

import bc.core.intrinsics;
import std.traits : isIntegral, isSigned;

pure @safe nothrow @nogc:

/// Calculates number of digits of the provided number including sign character for negative numbers.
int numDigits(T)(T number)
    if (isIntegral!T)
{
    pragma(inline);

    // handle negative numbers
    static if (isSigned!T)
    {
        if (_expect(number == long.min, false)) return 20; // for safety as it can't be converted to positive number
        if (number < 0) return 1 + numDigits(-long(number));
    }

    if (__ctfe)
    {
        // simple ctfe version
        import std.traits : Unqual;
        if (_expect(!number, false)) return 1; // special case for 0
        int digits = 0;
        alias UT = Unqual!T;
        UT n = number;
        while (n) { n /= 10; digits++; }
        return digits;
    }
    else
    {
        // specialization for 1B numbers
        static if (T.sizeof == 1)
        {
            static immutable ubyte[256] dmap = () {
                ubyte[256] ret;
                for (int i=0; i < 256; ++i) ret[i] = cast(ubyte)numDigits(i);
                return ret;
            }();
            return dmap[number];
        }
        else
        {
            if (_expect(number >= 10_000, false)) {
                if (number >= 10_000_000) {
                    if (number >= 100_000_000) {
                        if (number >= 1_000_000_000) {
                            static if (T.sizeof <= 4) return 10;
                            else {
                                // 8bit number branch
                                if (number >= 10_000_000_000) {
                                    if (number >= 100_000_000_000) {
                                        if (number >= 1_000_000_000_000) {
                                            if (number >= 10_000_000_000_000) {
                                                if (number >= 100_000_000_000_000) {
                                                    if (number >= 1_000_000_000_000_000) {
                                                        if (number >= 10_000_000_000_000_000) {
                                                            if (number >= 100_000_000_000_000_000) {
                                                                if (number >= 1_000_000_000_000_000_000) {
                                                                    if (number >= 10_000_000_000_000_000_000UL)
                                                                        return 20;
                                                                    return 19;
                                                                }
                                                                return 18;
                                                            }
                                                            return 17;
                                                        }
                                                        return 16;
                                                    }
                                                    return 15;
                                                }
                                                return 14;
                                            }
                                            return 13;
                                        }
                                        return 12;
                                    }
                                    return 11;
                                }
                                return 10;
                            }
                        }
                        return 9;
                    }
                    return 8;
                }
                if (number >= 100_000) {
                    if (number >= 1_000_000) return 7;
                    return 6;
                }
                return 5;
            }
            if (number >= 100) {
                if (number >= 1_000) return 4;
                return 3;
            }
            if (number >= 10) return 2;
            return 1;
        }
    }
}

///
@("numDigits CT")
@safe unittest
{
    // ctfe tests
    static assert(numDigits(0) == 1);
    static assert(numDigits(11) == 2);
    static assert(numDigits(-1) == 2);

    static assert(numDigits(int.min) == 11);
    static assert(numDigits(int.max) == 10);
    static assert(numDigits(long.min) == 20);
    static assert(numDigits(long.max) == 19);
    static assert(numDigits(ulong.min) == 1);
    static assert(numDigits(ulong.max) == 20);
}

///
@("numDigits RT")
@safe unittest
{
    // uint.max = 4_294_967_295 -> 10 digits
    // ulong.max = 18_446_744_073_709_551_615 -> 20 digits
    // long max = 9_223_372_036_854_775_807 -> 19 digits

    // rt tests
    assert(numDigits(0) == 1);
    assert(numDigits(11) == 2);
    assert(numDigits(-1) == 2);
    assert(numDigits(-123) == 4);

    assert(numDigits(int.min) == 11);
    assert(numDigits(int.max) == 10);
    assert(numDigits(long.min) == 20);
    assert(numDigits(long.max) == 19);
    assert(numDigits(ulong.min) == 1);
    assert(numDigits(ulong.max) == 20);

    long n = 10;
    foreach (i; 0..20)
    {
        import std.math : pow;
        assert(numDigits(pow(10UL, i)) == i+1);
    }
}
