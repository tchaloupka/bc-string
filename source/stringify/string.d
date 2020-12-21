/**
 * Some helper functions to work with strings
 */
module stringify.string;

import stringify.internal.memory : enforceMalloc, enforceRealloc, heapAlloc, heapDealloc;
import core.atomic : atomicOp;
import std.range : ElementEncodingType, hasLength, isInputRange;
import std.traits : ForeachType, isSomeChar, isSomeString, Unqual;
// debug import core.stdc.stdio;

alias CString = const(char)[];

/**
 * Temporary string buffer.
 * It can be used to build temporary \0 ended C strings.
 * For lengths < 255, it uses static char array, mallocated buffer otherwise.
 *
 * NOTE: be careful that pointer becomes invalid as soon as the struct comes out of scope!
 * NOTE: inspired by std.internal.cstring.TempCStringBuffer in Phobos library
 */
struct TempCString(C)
{
    @trusted pure nothrow @nogc:

    @disable this();
    @disable this(this);
    alias ptr this;

    @property inout(C)* bufPtr() inout
    {
        return _ptr == useStack ? _buf.ptr : _ptr;
    }

    @property const(C)* ptr() const { return bufPtr; }
    const(C)[] opIndex() const pure { return bufPtr[0 .. _length]; }

    ~this()
    {
        if (_ptr != useStack)
        {
            import core.memory : pureFree;
            pureFree(_ptr);
        }
    }

    private:
    C* _ptr;
    size_t _length;
    C[256] _buf;

    enum C* useStack = () @trusted { return cast(C*)size_t.max; }();
    static TempCString initialize() { TempCString res = void; return res; }
}

/// ditto
auto tempCString(C = char, S)(scope S str) if (isSomeChar!C && (isInputRange!S || isSomeString!S) &&
    isSomeChar!(ElementEncodingType!S))
{
    alias CF = Unqual!(ElementEncodingType!S);
    auto res = TempCString!C.initialize();

    static if (isSomeString!S)
    {
        if (str is null)
        {
            res._length = 0;
            res._ptr = null;
            return res;
        }
    }

    static if (C.sizeof == CF.sizeof && is(typeof(res._buf[0 .. str.length] = str[])))
    {
        if (str.length < res._buf.length)
        {
            res._buf[0..str.length] = str[];
            res._buf[str.length] = 0;
            res._ptr = res.useStack;
        }
        else
        {
            res._ptr = () @trusted {
                auto p = cast(C*)enforceMalloc((str.length + 1) * C.sizeof);
                p[0 .. str.length] = str[];
                p[str.length] = 0;
                return cast(C*)p;
            }();
        }
        res._length = str.length;
        return res;
    }
    else
    {
        static assert(!(isSomeString!S && CF.sizeof == C.sizeof), "Should be using slice assignment.");
        C[] p = res._buf;
        size_t i;

        size_t strLength;
        static if (hasLength!S) strLength = str.length;

        import std.utf : byUTF;
        static if (isSomeString!S)
            auto r = cast(const(CF)[])str;  // because inout(CF) causes problems with byUTF
        else
            alias r = str;

        C[] heapBuffer;
        foreach (const c; r.byUTF!(Unqual!C))
        {
            if (i + 1 == p.length)
            {
                heapBuffer = trustedRealloc(p, strLength, heapBuffer is null);
                p = heapBuffer;
            }
            p[i++] = c;
        }
        p[i] = 0;
        res._length = i;
        res._ptr = (heapBuffer is null ? res.useStack : &heapBuffer[0]);
        return res;
    }
}

///
nothrow @nogc @system unittest
{
    import core.stdc.string : strlen;

    string str = "abc";

    // Intended usage
    assert(strlen(str.tempCString()) == 3);

    // Correct usage
    auto tmp = str.tempCString();
    assert(strlen(tmp) == 3); // or `tmp.ptr`, or `tmp.buffPtr`

    // $(RED WARNING): $(RED Incorrect usage)
    auto pInvalid1 = str.tempCString().ptr;
    const char* pInvalid2 = str.tempCString();
}

nothrow @nogc @safe unittest
{
    import std.algorithm : filter;
    import std.utf : byCodeUnit;

    auto tmp = "baz".byCodeUnit.filter!(a => a == 'z').tempCString;
    assert(tmp._length == 1);
    assert(tmp._buf[0] == 'z');
    assert(tmp._buf[1] == '\0');
}

nothrow @nogc @safe unittest
{
    {
        import std.algorithm : filter;
        import std.utf : byCodeUnit;

        auto tmp = "baz".byCodeUnit.filter!(a => a == 'z').tempCString!wchar;
        assert(tmp._length == 1);
        assert(tmp._buf[0] == 'z');
        assert(tmp._buf[1] == '\0');
    }

    {
        auto tmp = "baz".tempCString!dchar;
        assert(tmp._buf[0..3] == "baz"d);
    }
}

nothrow @nogc @system unittest
{
    import core.stdc.string : strlen;
    import std.algorithm : joiner;
    import std.conv : text;
    import std.range : repeat;

    static immutable str = "foo".repeat(100).joiner.text;
    assert(strlen(str.tempCString()) == 3 * 100);
}

/**
 * Refcounted String implementation.
 *
 * It uses malloc for string buffer and can be used directly as a C string as it manages ending \0 internally.
 * Payload is reference counted so content is destroyed with last reference.
 * Can be used as a string builder too.
 *
 * NOTE: Beware of using exposed data pointer stored before some more content is added to RCString as internal buffer can be reallocated / resized if needed.
 */
alias RCString = StringImpl!(char, RC.yes);

/// ditto
alias RCStringW = StringImpl!(wchar, RC.yes);

/// ditto
alias RCStringD = StringImpl!(dchar, RC.yes);

/**
 * String with unique ownership implementation
 *
 * Similar to RCString but can be only moved passing it's ownership.
 */
alias String = StringImpl!(char, RC.no);

/// ditto
alias WString = StringImpl!(wchar, RC.no);

/// ditto
alias DString = StringImpl!(dchar, RC.no);

private enum RC { no, yes }

private struct StringImpl(C, RC rc)
{
    @safe nothrow @nogc:

    static if (rc)
    {
        private
        {
            struct Payload
            {
                shared size_t refs;
                size_t len;
                C[] buf;

                ~this() @trusted pure nothrow @nogc
                {
                    import core.memory : pureFree;
                    if (buf) pureFree(buf.ptr);
                }
            }

            Payload* pay;
        }

        /// Copy constructor
        this(ref return scope StringImpl rhs) pure
        {
            if (rhs.pay)
            {
                this.pay = rhs.pay;
                atomicOp!"+="(this.pay.refs, 1);
            }
        }

        /// Destructor
        ~this()
        {
            if (pay && atomicOp!"-="(pay.refs, 1) == 0) heapDealloc(pay);
        }
    }
    else
    {
        private
        {
            size_t len;
            C[] buf;
            alias pay = typeof(this); // to access fields through pay.xx too
        }

        ~this() pure @trusted
        {
            import core.memory : pureFree;
            if (buf) pureFree(buf.ptr);
        }

        @disable this(this);
    }

    /**
     * Constructor for cases when we know prior to the creation total length of the future string.
     * It preallocates internal buffer with `initialSize + 1` size (+1 for terminal \0).
     */
    this(size_t initialSize) pure
    {
        static if (rc) pay = heapAlloc!Payload(1, 0);
        immutable len = initialSize + 1;
        pay.buf = () @trusted { return (cast(C*)enforceMalloc(len * C.sizeof))[0..len]; }();
    }

    this(S)(auto ref scope S str) if (isInputRange!(Unqual!S) && isSomeChar!(Unqual!(ForeachType!S)))
    {
        put(str);
    }

    /**
     * Creates RCString from the provided arguments formated to string with nogcFormatter
     */
    static StringImpl from(ARGS...)(auto ref ARGS args)
    {
        import stringify.format : getFormatSize, nogcFormatTo;

        size_t total;
        // calculate total size needed so we don't have to reallocate
        static foreach (a; args) total += getFormatSize(a);

        // and format arguments to RCString
        auto ret = StringImpl(total);
        static foreach (a; args) ret.nogcFormatTo(a);
        return ret;
    }

    alias data this;

    /// Access internal string
    @property inout(C)[] data() pure inout
    {
        if (!length) return null;

        assert(pay.buf);
        return pay.buf[0..pay.len];
    }

    @property inout(C*) ptr() pure inout @trusted
    {
        if (!length) return null;
        return pay.buf.ptr;
    }

    /// Slicing support for the internal buffer data
    @property inout(C)[] opSlice() pure inout
    {
        return this.data;
    }

    /// ditto
    @property inout(C)[] opSlice(size_t start, size_t end) pure inout
    {
        if (start > length || end > length) assert(0, "Index out of bounds");
        if (start > end) assert(0, "Invalid slice indexes");
        return this.data[start .. end];
    }

    /// Indexed access to the buffer data
    @property ref C opIndex(size_t idx) pure return
    {
        if (idx >= length) assert(0, "Index out of bounds");
        return this.data[idx];
    }

    /// opDollar implementation
    alias length opDollar;

    @property size_t length() pure const
    {
        static if (rc)
            return pay ? pay.len : 0;
        else
            return len;
    }

    /**
     * Clears content of the data, but keeps internal buffer as is so it can be used to build another string.
     */
    void clear() pure
    {
        static if (rc) { if (pay) pay.len = 0; }
        else len = 0;
    }

    alias opOpAssign(string op : "~") = put;

    void put(in C val) pure
    {
        ensureAvail(1);
        pay.buf[pay.len++] = val;
        pay.buf[pay.len] = 0;
    }

    void put(S)(auto ref scope S str) if (isInputRange!(Unqual!S) && isSomeChar!(Unqual!(ForeachType!S)))
    {
        alias CF = Unqual!(ElementEncodingType!S);

        static if (C.sizeof == CF.sizeof && is(typeof(pay.buf[0 .. str.length] = str[])))
        {
            ensureAvail(str.length);
            pay.buf[pay.len .. pay.len + str.length] = str[];
            pay.len += str.length;
            pay.buf[pay.len] = 0;
        }
        else
        {
            // copy range
            static if (hasLength!S) ensureAvail(str.length);
            import std.utf : byUTF;
            static if (isSomeString!S)
                auto r = cast(const(CF)[])str;  // because inout(CF) causes problems with byUTF
            else
                alias r = str;

            foreach (ch; r.byUTF!(Unqual!C))
            {
                static if (!hasLength!S) ensureAvail(1);
                pay.buf[pay.len++] = ch;
            }
            pay.buf[pay.len] = 0;
        }
    }

    private void ensureAvail(size_t sz) pure
    {
        static if (__VERSION__ >= 2094) pragma(inline, true);
        else pragma(inline);
        import core.bitop : bsr;
        import std.algorithm : max, min;

        static if (rc)
        {
            if (!pay)
            {
                // allocate new payload with required size
                pay = heapAlloc!Payload(1, 0);
                immutable l = max(sz+1, 8); // allocates at leas 8B
                pay.buf = () @trusted { return (cast(C*)enforceMalloc(l * C.sizeof))[0..l]; }();
                return;
            }
        }
        else
        {
            if (buf is null)
            {
                immutable l = max(sz+1, 8); // allocates at leas 8B
                buf = () @trusted { return (cast(C*)enforceMalloc(l * C.sizeof))[0..l]; }();
                return;
            }
        }

        if (pay.buf.length - pay.len > sz) return; // we can fit in what we've already allocated

        // reallocate buffer
        // Note: new length calculation taken from std.array.appenderNewCapacity
        immutable ulong mult = 100 + (1000UL) / (bsr((pay.len + sz)) + 1);
        immutable l = cast(size_t)(((pay.len + sz) * min(mult, 200) + 99) / 100);
        // debug printf("realloc %lu -> %lu\n", pay.len, l);
        pay.buf = () @trusted { return (cast(C*)enforceRealloc(pay.buf.ptr, l * C.sizeof))[0..l]; }();
    }
}

auto rcString(C = char, S)(auto ref S str)
{
    StringImpl!(C, RC.yes) ret;
    ret.put(str);
    return ret;
}

@system @nogc unittest
{
    import std.algorithm : filter;
    import std.utf : byCodeUnit;

    RCString s;
    s ~= "fo";
    assert(s.pay.len == 2);
    assert(s.pay.buf.length >= 3);

    s ~= 'o';
    assert(s.pay.len == 3);
    assert(s.pay.buf.length >= 4);

    s ~= "bar";
    assert(s.pay.len == 6);
    assert(s.pay.buf.length >= 7);

    s ~= "baz".byCodeUnit.filter!(a => a == 'z');
    assert(s.length == "foobarz".length);
    assert(s.data == "foobarz");
    assert(s == "foobarz");
    assert(s.ptr == &s.data[0]);
    assert((s.ptr["foobarz".length]) == 0);
}

@nogc unittest
{
    auto str = RCString.from("foo", 42, "bar");
    assert(str == "foo42bar");
}

@nogc unittest
{
    auto str = RCStringW.from("foo");
    assert(str == "foo"w);
}

@nogc unittest
{
    auto str = "foo".rcString();
    assert(str == "foo");
}

private C[] trustedRealloc(C)(scope C[] buf, size_t strLength, bool bufIsOnStack)
    @trusted @nogc pure nothrow
{
    pragma(inline, false);  // because it's rarely called

    import stringify.internal.memory : enforceMalloc, enforceRealloc;

    size_t newlen = buf.length * 3 / 2;

    if (bufIsOnStack)
    {
        if (newlen <= strLength)
            newlen = strLength + 1; // +1 for terminating 0
        auto ptr = cast(C*) enforceMalloc(newlen * C.sizeof);
        ptr[0 .. buf.length] = buf[];
        return ptr[0 .. newlen];
    }
    else
    {
        if (buf.length >= size_t.max / (2 * C.sizeof))
        {
            version (D_Exceptions)
            {
                import core.exception : onOutOfMemoryError;
                onOutOfMemoryError();
            }
            else assert(0, "Memory allocation failed");
        }
        auto ptr = cast(C*) enforceRealloc(buf.ptr, newlen * C.sizeof);
        return ptr[0 .. newlen];
    }
}

/**
 * Alternative implementation of `std.string.outdent` that differs in:
 *
 *   * if first line is not indented, other lines are dedented still (std.string.outdent returns original text in that case)
 *   * empty lines at the text start are removed
 *   * is a bit faster
 *   * CT template to dedent string literals
 */

template dedentCT(alias str)
{
    enum dedentCT = dedent(str);
}

/// ditto
S dedent(S)(S str) pure @safe nothrow
    if (isSomeString!S)
{
    import std.array : join;
    import std.range : empty;
    import std.string : chomp, splitLines, stripLeft;
    import std.typecons : Yes;

    auto lines = str.stripLeft.splitLines(Yes.keepTerminator);
    if (lines.empty) return null;

    size_t shortestIndent = size_t.max;

    // special case for first line - ignore no indentation on it
    auto flstrip = lines[0].stripLeft();
    if (flstrip.empty) lines[0] = lines[0][lines[0].chomp().length .. $]; // strip empty lines from whitespace
    else
    {
        immutable ilen = lines[0][0 .. $-flstrip.length].length;
        if (ilen) shortestIndent = ilen;
    }

    foreach (ref line; lines[1..$])
    {
        const stripped = line.stripLeft();
        if (stripped.empty) line = line[line.chomp().length .. $]; // strip empty lines from whitespace
        else
        {
            const indent = line[0 .. $-stripped.length].length;
            if (indent < shortestIndent)
            {
                if (indent == 0) return lines.join; // cant remove any indentation
                shortestIndent = indent;
            }
        }
    }

    if (shortestIndent == size_t.max) return lines.join;

    // special case for potentially not indented first line
    flstrip = lines[0].stripLeft();
    if (flstrip.length && flstrip.length < lines[0].length) lines[0] = lines[0][shortestIndent..$];

    foreach (ref line; lines[1..$])
    {
        if (line.length <= shortestIndent) continue; // Do nothing
        line = line[shortestIndent..$];
    }

    return lines.join;
}

unittest
{
    // with empty first line
    {
        string str1 = `
                DELETE FROM elements.element
                WHERE id=ANY($1) AND type_id IN (
                    SELECT id FROM elements.element_type WHERE owner=$2
                )`;

        string str2 =
                    "DELETE FROM elements.element\n" ~
                    "WHERE id=ANY($1) AND type_id IN (\n" ~
                    "    SELECT id FROM elements.element_type WHERE owner=$2\n" ~
                    ")";

        assert(dedent(str1) == dedent(str2));
        assert(dedent(str1) == str2);
        assert(dedent(str2) != str1);
    }

    {
        string str1 = `DELETE FROM elements.element
                WHERE id=ANY($1) AND type_id IN (
                    SELECT id FROM elements.element_type WHERE owner=$2
                )`;

        string str2 = "DELETE FROM elements.element\n" ~
                    "WHERE id=ANY($1) AND type_id IN (\n" ~
                    "    SELECT id FROM elements.element_type WHERE owner=$2\n" ~
                    ")";

        assert(dedent(str1) == dedent(str2));
        assert(dedent(str1) == str2);
        assert(dedent(str2) != str1);
    }

    // test that we didn't touch number of lines
    {
        assert(dedentCT!`
            2
            3
        ` == "2\n3\n"); // first line is dropped, last newline is kept
    }

    // test we don't dedent when some line is not indented
    {
        enum str = `aa
            bb
cc`;
        assert(dedentCT!str == str);
    }

    // test that we don't touch space after last line text
    {
        assert(dedentCT!"  foo " == "foo ");
        assert(dedentCT!`foo
            bar ` == "foo\nbar ");
    }
}
