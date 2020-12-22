/**
 * Normally just a public import of std.utf, but for betterC internally used functions are added
 * here (stripped from unneeded stuff not compatible with betterC).
 *
 * For details see: https://github.com/dlang/phobos/blob/master/std/utf.d
 *
 * Last revision from commit: 9351e91fe466e315baa6c82205d2563ce80e4c91
 */
module bc.internal.utf;

version (D_BetterC) version = BC_UTF;
else
    public import std.utf;

version (BC_UTF):

import std.meta : AliasSeq;
import std.range.primitives;
import std.traits : isAutodecodableString, isConvertibleToString, isSomeChar, isSomeString;

@safe pure nothrow @nogc:

enum dchar replacementDchar = '\uFFFD';

bool isValidDchar(dchar c) pure nothrow @safe @nogc
{
    return c < 0xD800 || (c > 0xDFFF && c <= 0x10FFFF);
}

// workaround for non working std.traits isInputRange
template isDecodableRange(R)
{
    enum isDecodableRange =
        isInputRange!R ||
        (
            isAutodecodableString!R && !__traits(hasMember, R, "empty") &&
            !__traits(hasMember, R, "front") && !__traits(hasMember, R, "popFront")
        );
}

auto byCodeUnit(R)(R r)
if ((isConvertibleToString!R && !isStaticArray!R) ||
    (isDecodableRange!R && isSomeChar!(ElementEncodingType!R)))
{
    import std.traits : StringTypeOf;
    static if ((isAutodecodableString!R && !__traits(hasMember, R, "empty") &&
                !__traits(hasMember, R, "front") && !__traits(hasMember, R, "popFront")))
    {
        static struct ByCodeUnitImpl
        {
        @safe pure nothrow @nogc:

            @property bool empty() const     { return source.length == 0; }
            @property auto ref front() inout { return source[0]; }
            void popFront()                  { source = source[1 .. $]; }

            @property auto save() { return ByCodeUnitImpl(source.save); }

            @property auto ref back() inout { return source[$ - 1]; }
            void popBack()                  { source = source[0 .. $-1]; }

            auto ref opIndex(size_t index) inout     { return source[index]; }
            auto opSlice(size_t lower, size_t upper) { return ByCodeUnitImpl(source[lower .. upper]); }

            @property size_t length() const { return source.length; }
            alias opDollar = length;

            StringTypeOf!R source;
        }

        static assert(isRandomAccessRange!ByCodeUnitImpl);

        return ByCodeUnitImpl(r);
    }
    else static if (!isInputRange!R ||
                    (is(R : const dchar[]) && !__traits(hasMember, R, "empty") &&
                    !__traits(hasMember, R, "front") && !__traits(hasMember, R, "popFront")))
    {
        return cast(StringTypeOf!R) r;
    }
    else return r; // byCodeUnit for ranges and dchar[] is a no-op
}

template byUTF(C) if (isSomeChar!C)
{
    static if (is(immutable C == immutable UC, UC) && !is(C == UC))
        alias byUTF = byUTF!UC;
    else:

    auto ref byUTF(R)(R r)
        if (isAutodecodableString!R && isDecodableRange!R && isSomeChar!(ElementEncodingType!R))
    {
        return byUTF(r.byCodeUnit());
    }

    auto ref byUTF(R)(R r)
        if (!isAutodecodableString!R && isDecodableRange!R && isSomeChar!(ElementEncodingType!R))
    {
        static if (is(immutable ElementEncodingType!R == immutable RC, RC) && is(RC == C))
        {
            return r.byCodeUnit();
        }
        else static if (is(C == dchar))
        {
            static struct Result
            {
                enum Empty = uint.max;  // range is empty or just constructed

                this(return R r) { this.r = r; }
                this(return R r, uint buff)
                {
                    this.r = r;
                    this.buff = buff;
                }

                @property bool empty()
                {
                    return buff == Empty && r.empty;
                }

                @property dchar front() scope // 'scope' required by call to decodeFront() below
                {
                    if (buff == Empty)
                    {
                        auto c = r.front;

                        static if (is(RC == wchar))
                            enum firstMulti = 0xD800; // First high surrogate.
                        else
                            enum firstMulti = 0x80; // First non-ASCII.
                        if (c < firstMulti)
                        {
                            r.popFront;
                            buff = cast(dchar) c;
                        }
                        else
                            buff = () @trusted { return decodeFront(r); }();
                    }
                    return cast(dchar) buff;
                }

                void popFront()
                {
                    if (buff == Empty)
                        front();
                    buff = Empty;
                }

                static if (isForwardRange!R)
                {
                    @property auto save()
                    {
                        return Result(r.save, buff);
                    }
                }

            private:

                R r;
                uint buff = Empty;      // one character lookahead buffer
            }

            return Result(r);
        }
        else
        {
            static struct Result
            {
                this(return R r)
                {
                    this.r = r;
                }

                this(return R r, ushort pos, ushort fill, C[4 / C.sizeof] buf)
                {
                    this.r = r;
                    this.pos = pos;
                    this.fill = fill;
                    this.buf = buf;
                }

                @property bool empty()
                {
                    return pos == fill && r.empty;
                }

                @property auto front() scope // 'scope' required by call to decodeFront() below
                {
                    if (pos == fill)
                    {
                        pos = 0;
                        auto c = r.front;

                        static if (C.sizeof >= 2 && RC.sizeof >= 2)
                            enum firstMulti = 0xD800; // First high surrogate.
                        else
                            enum firstMulti = 0x80; // First non-ASCII.
                        if (c < firstMulti)
                        {
                            fill = 1;
                            r.popFront;
                            buf[pos] = cast(C) c;
                        }
                        else
                        {
                            static if (is(RC == dchar))
                            {
                                r.popFront;
                                dchar dc = c;
                            }
                            else
                                dchar dc = () @trusted { return decodeFront(r); }();
                            fill = cast(ushort) encode(buf, dc);
                        }
                    }
                    return buf[pos];
                }

                void popFront()
                {
                    if (pos == fill)
                        front;
                    ++pos;
                }

                static if (isForwardRange!R)
                {
                    @property auto save()
                    {
                        return Result(r.save, pos, fill, buf);
                    }
                }

            private:

                R r;
                ushort pos, fill;
                C[4 / C.sizeof] buf = void;
            }

            return Result(r);
        }
    }
}

dchar decodeFront(S)(ref S str, out size_t numCodeUnits)
if (!isSomeString!S && isInputRange!S && isSomeChar!(ElementType!S))
in { assert(!str.empty); }
out (result) { assert(isValidDchar(result)); }
do
{
    immutable fst = str.front;

    if (fst < codeUnitLimit!S)
    {
        str.popFront();
        numCodeUnits = 1;
        return fst;
    }
    else
    {
        // https://issues.dlang.org/show_bug.cgi?id=14447 forces canIndex to be
        // done outside of decodeImpl, which is undesirable, since not all
        // overloads of decodeImpl need it. So, it should be moved back into
        // decodeImpl once https://issues.dlang.org/show_bug.cgi?id=8521
        // has been fixed.
        enum canIndex = is(S : const char[]) || isRandomAccessRange!S && hasSlicing!S && hasLength!S;
        immutable retval = decodeImpl!(canIndex)(str, numCodeUnits);

        // The other range types were already popped by decodeImpl.
        static if (isRandomAccessRange!S && hasSlicing!S && hasLength!S)
            str = str[numCodeUnits .. str.length];

        return retval;
    }
}

/// ditto
dchar decodeFront(S)(ref S str, out size_t numCodeUnits) @trusted pure
if (isSomeString!S)
in { assert(!str.empty); }
out (result) { assert(isValidDchar(result)); }
do
{
    if (str[0] < codeUnitLimit!S)
    {
        numCodeUnits = 1;
        immutable retval = str[0];
        str = str[1 .. $];
        return retval;
    }
    else static if (is(immutable S == immutable C[], C))
    {
        immutable retval = decodeImpl!true(cast(const(C)[]) str, numCodeUnits);
        str = str[numCodeUnits .. $];
        return retval;
    }
}

/// ditto
dchar decodeFront(S)(ref S str) if (isInputRange!S && isSomeChar!(ElementType!S))
{
    size_t numCodeUnits;
    return decodeFront(str, numCodeUnits);
}

// Gives the maximum value that a code unit for the given range type can hold.
package template codeUnitLimit(S)
if (isSomeChar!(ElementEncodingType!S))
{
    static if (is(immutable ElementEncodingType!S == immutable char))
        enum char codeUnitLimit = 0x80;
    else static if (is(immutable ElementEncodingType!S == immutable wchar))
        enum wchar codeUnitLimit = 0xD800;
    else
        enum dchar codeUnitLimit = 0xD800;
}

private dchar decodeImpl(bool canIndex, S)(auto ref S str, ref size_t index)
if (is(S : const char[]) || (isInputRange!S && is(immutable ElementEncodingType!S == immutable char)))
{
    /* The following encodings are valid, except for the 5 and 6 byte
     * combinations:
     *  0xxxxxxx
     *  110xxxxx 10xxxxxx
     *  1110xxxx 10xxxxxx 10xxxxxx
     *  11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
     *  111110xx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
     *  1111110x 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
     */

    /* Dchar bitmask for different numbers of UTF-8 code units.
     */
    alias bitMask = AliasSeq!((1 << 7) - 1, (1 << 11) - 1, (1 << 16) - 1, (1 << 21) - 1);

    static if (is(S : const char[]))
        auto pstr = str.ptr + index;    // this is what makes decodeImpl() @system code
    else static if (isRandomAccessRange!S && hasSlicing!S && hasLength!S)
        auto pstr = str[index .. str.length];
    else
        alias pstr = str;

    // https://issues.dlang.org/show_bug.cgi?id=14447 forces this to be done
    // outside of decodeImpl
    //enum canIndex = is(S : const char[]) || (isRandomAccessRange!S && hasSlicing!S && hasLength!S);

    static if (canIndex)
    {
        immutable length = str.length - index;
        ubyte fst = pstr[0];
    }
    else
    {
        ubyte fst = pstr.front;
        pstr.popFront();
    }

    if ((fst & 0b1100_0000) != 0b1100_0000)
    {
        ++index;            // always consume bad input to avoid infinite loops
        return replacementDchar;
    }
    ubyte tmp = void;
    dchar d = fst; // upper control bits are masked out later
    fst <<= 1;

    foreach (i; AliasSeq!(1, 2, 3))
    {
        static if (canIndex)
        {
            if (i == length)
            {
                index += i;
                return replacementDchar;
            }
        }
        else
        {
            if (pstr.empty)
            {
                index += i;
                return replacementDchar;
            }
        }

        static if (canIndex)
            tmp = pstr[i];
        else
        {
            tmp = pstr.front;
            pstr.popFront();
        }

        if ((tmp & 0xC0) != 0x80)
        {
            index += i + 1;
            return replacementDchar;
        }

        d = (d << 6) | (tmp & 0x3F);
        fst <<= 1;

        if (!(fst & 0x80)) // no more bytes
        {
            d &= bitMask[i]; // mask out control bits

            // overlong, could have been encoded with i bytes
            if ((d & ~bitMask[i - 1]) == 0)
            {
                index += i + 1;
                return replacementDchar;
            }

            // check for surrogates only needed for 3 bytes
            static if (i == 2)
            {
                if (!isValidDchar(d))
                {
                    index += i + 1;
                    return replacementDchar;
                }
            }

            index += i + 1;
            static if (i == 3)
            {
                if (d > dchar.max) d = replacementDchar;
            }
            return d;
        }
    }

    index += 4;             // read 4 chars by now
    return replacementDchar;
}

private dchar decodeImpl(bool canIndex, S)(auto ref S str, ref size_t index)
if (is(S : const wchar[]) || (isInputRange!S && is(immutable ElementEncodingType!S == immutable wchar)))
{
    static if (is(S : const wchar[]))
        auto pstr = str.ptr + index;
    else static if (isRandomAccessRange!S && hasSlicing!S && hasLength!S)
        auto pstr = str[index .. str.length];
    else
        alias pstr = str;

    // https://issues.dlang.org/show_bug.cgi?id=14447 forces this to be done
    // outside of decodeImpl
    //enum canIndex = is(S : const wchar[]) || (isRandomAccessRange!S && hasSlicing!S && hasLength!S);

    static if (canIndex)
    {
        immutable length = str.length - index;
        uint u = pstr[0];
    }
    else
    {
        uint u = pstr.front;
        pstr.popFront();
    }

    // The < case must be taken care of before decodeImpl is called.
    assert(u >= 0xD800);

    if (u <= 0xDBFF)
    {
        static if (canIndex)
            immutable onlyOneCodeUnit = length == 1;
        else
            immutable onlyOneCodeUnit = pstr.empty;

        if (onlyOneCodeUnit)
        {
            ++index;
            return replacementDchar;
        }

        static if (canIndex)
            immutable uint u2 = pstr[1];
        else
        {
            immutable uint u2 = pstr.front;
            pstr.popFront();
        }

        if (u2 < 0xDC00 || u2 > 0xDFFF)
            u = replacementDchar;
        else
            u = ((u - 0xD7C0) << 10) + (u2 - 0xDC00);
        ++index;
    }
    else if (u >= 0xDC00 && u <= 0xDFFF)
        u = replacementDchar;

    ++index;

    // Note: u+FFFE and u+FFFF are specifically permitted by the
    // Unicode standard for application internal use (see isValidDchar)

    return cast(dchar) u;
}

private dchar decodeImpl(bool canIndex, S)(auto ref S str, ref size_t index)
if (is(S : const dchar[]) || (isInputRange!S && is(immutable ElementEncodingType!S == immutable dchar)))
{
    static if (is(S : const dchar[]))
        auto pstr = str.ptr;
    else
        alias pstr = str;

    static if (is(S : const dchar[]) || isRandomAccessRange!S)
    {
        dchar dc = pstr[index];
        if (!isValidDchar(dc))
            dc = replacementDchar;

        ++index;
        return dc;
    }
    else
    {
        dchar dc = pstr.front;
        if (!isValidDchar(dc))
            dc = replacementDchar;

        ++index;
        pstr.popFront();
        return dc;
    }
}

size_t encode()(out char[4] buf, dchar c) @safe pure
{
    if (c <= 0x7F)
    {
        assert(isValidDchar(c));
        buf[0] = cast(char) c;
        return 1;
    }
    if (c <= 0x7FF)
    {
        assert(isValidDchar(c));
        buf[0] = cast(char)(0xC0 | (c >> 6));
        buf[1] = cast(char)(0x80 | (c & 0x3F));
        return 2;
    }
    if (c <= 0xFFFF)
    {
        if (0xD800 <= c && c <= 0xDFFF)
            c = replacementDchar;

        assert(isValidDchar(c));
    L3:
        buf[0] = cast(char)(0xE0 | (c >> 12));
        buf[1] = cast(char)(0x80 | ((c >> 6) & 0x3F));
        buf[2] = cast(char)(0x80 | (c & 0x3F));
        return 3;
    }
    if (c <= 0x10FFFF)
    {
        assert(isValidDchar(c));
        buf[0] = cast(char)(0xF0 | (c >> 18));
        buf[1] = cast(char)(0x80 | ((c >> 12) & 0x3F));
        buf[2] = cast(char)(0x80 | ((c >> 6) & 0x3F));
        buf[3] = cast(char)(0x80 | (c & 0x3F));
        return 4;
    }

    assert(!isValidDchar(c));
    c = replacementDchar;
    goto L3;
}

size_t encode()(out wchar[2] buf, dchar c) @safe pure
{
    if (c <= 0xFFFF)
    {
        if (0xD800 <= c && c <= 0xDFFF)
            c = replacementDchar;

        assert(isValidDchar(c));
    L1:
        buf[0] = cast(wchar) c;
        return 1;
    }
    if (c <= 0x10FFFF)
    {
        assert(isValidDchar(c));
        buf[0] = cast(wchar)((((c - 0x10000) >> 10) & 0x3FF) + 0xD800);
        buf[1] = cast(wchar)(((c - 0x10000) & 0x3FF) + 0xDC00);
        return 2;
    }

    c = replacementDchar;
    goto L1;
}

size_t encode()(out dchar[1] buf, dchar c) @safe pure
{
    if ((0xD800 <= c && c <= 0xDFFF) || 0x10FFFF < c)
        c = replacementDchar;
    else
        assert(isValidDchar(c));
    buf[0] = c;
    return 1;
}
