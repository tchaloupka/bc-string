/**
 * Some helper functions to work with strings
 */
module bc.string.string;

import bc.core.intrinsics;
import bc.core.memory : enforceMalloc, enforceRealloc, heapAlloc, heapDealloc;
import bc.internal.range : ElementEncodingType, hasLength, isInputRange;
import std.traits : ForeachType, isSomeChar, isSomeString, isStaticArray, Unqual;
// debug import core.stdc.stdio;

nothrow @nogc:

alias CString = const(char)[];

template isAcceptableString(S)
{
    enum isAcceptableString =
        (isInputRange!S || isSomeString!S || isStaticArray!S) &&
        isSomeChar!(ElementEncodingType!S);
}

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

    @property inout(C)* bufPtr() return inout
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
auto tempCString(C = char, S)(scope S str) if (isAcceptableString!S)
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

        import bc.internal.utf : byUTF;
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
@("tempCString")
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

@("tempCString - char, wchar, dchar")
nothrow @nogc @trusted unittest
{
    import std.algorithm : filter;
    import bc.internal.utf : byCodeUnit;

    {
        auto tmp = "baz".byCodeUnit.filter!(a => a == 'z').tempCString;
        assert(tmp._length == 1);
        assert(tmp._buf[0] == 'z');
        assert(tmp._buf[1] == '\0');
    }

    {
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

@("tempCString - static array")
nothrow @nogc @trusted unittest
{
    import core.stdc.string : strlen;

    immutable(char)[3] str = "abc";
    assert(strlen(str.tempCString()) == 3);
}

/**
 * Refcounted string implementation.
 *
 * It uses malloc for string buffer.
 *
 * Types with `RC` prefix are reference counted, so they can be moved around freely.
 * Types without `RC` prefix has disabled copy constructor and can be only moved (passing ownership) or cloned.
 *
 * There are wariants with `W` and `D` before `String` that corresponds to payloads `wchar` and `dchar` as usual.
 *
 * Types that ends with `Z` means that they internally manages trailing '\0' and so can be safely used with C interop.
 *
 * NOTE: Beware of using exposed data pointer stored before some more content is added to RCString as internal buffer can be reallocated / resized if needed.
 */
alias RCString = StringImpl!(char, RC.yes, Zero.no);

/// ditto
alias RCWString = StringImpl!(wchar, RC.yes, Zero.no);

/// ditto
alias RCDString = StringImpl!(dchar, RC.yes, Zero.no);

/// ditto
alias RCStringZ = StringImpl!(char, RC.yes, Zero.yes);

/// ditto
alias RCWStringZ = StringImpl!(wchar, RC.yes, Zero.yes);

/// ditto
alias RCDStringZ = StringImpl!(dchar, RC.yes, Zero.yes);

/**
 * String with unique ownership implementation.
 *
 * Similar to RCString but can be only moved passing it's ownership.
 * Furthermore it uses 512B stack allocated buffer for short strings.
 */
alias String = StringImpl!(char, RC.no, Zero.no);

/// ditto
alias WString = StringImpl!(wchar, RC.no, Zero.no);

/// ditto
alias DString = StringImpl!(dchar, RC.no, Zero.no);

/// ditto
alias StringZ = StringImpl!(char, RC.no, Zero.yes);

/// ditto
alias WStringZ = StringImpl!(wchar, RC.no, Zero.yes);

/// ditto
alias DStringZ = StringImpl!(dchar, RC.no, Zero.yes);

private enum RC { no, yes }
private enum Zero { no, yes }

private struct StringImpl(C, RC rc, Zero zero)
{
    @safe nothrow @nogc:

    static if (zero) enum Z = 1;
    else enum Z = 0;

    static if (rc)
    {
        private
        {
            struct Payload
            {
                size_t refs;
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
        this(ref return scope inout StringImpl rhs) pure @safe inout
        {
            pay = rhs.pay;
            if (pay) () @trusted { (cast(Payload*)pay).refs++; }();
        }

        /// Destructor
        ~this()
        {
            if (pay && --pay.refs == 0) heapDealloc(pay);
        }
    }
    else
    {
        private
        {
            enum STACK_LEN = 512;
            size_t len;
            C[STACK_LEN] stackBuf;
            C[] buf;
            bool useStackBuf;
            alias pay = typeof(this); // to access fields through pay.xx too
        }

        ~this() pure @trusted
        {
            import core.memory : pureFree;
            if (buf) pureFree(buf.ptr);
        }

        @disable this(this);

        // constructor used by move
        private this(C[] sbuf, C[] buf, size_t len)
        {
            this.stackBuf[0..sbuf.length] = sbuf[];
            this.buf = buf;
            this.len = len;
        }

        StringImpl move() scope @trusted
        {
            import std.algorithm : min;
            auto obuf = buf;
            auto olen = len;
            buf = null;
            len = 0;
            return StringImpl(stackBuf[0..min(STACK_LEN, olen)], obuf, olen);
        }

        ///
        StringImpl clone() scope @trusted
        {
            return StringImpl(this[]);
        }
    }

    /**
     * Constructor for cases when we know prior to the creation total length of the future string.
     * It preallocates internal buffer with `initialSize`.
     */
    this(size_t initialSize) pure
    {
        static if (rc) pay = heapAlloc!Payload(1, 0);
        immutable len = initialSize + Z;
        static if (!rc) {
            if (len <= STACK_LEN) return; // we can use stack buffer for that
        }
        pay.buf = () @trusted { return (cast(C*)enforceMalloc(len * C.sizeof))[0..len]; }();
    }

    this(S)(auto ref scope S str)
    {
        put(str);
    }

    /**
     * Creates RCString from the provided arguments formated to string with nogcFormatter
     */
    static StringImpl from(ARGS...)(auto ref ARGS args)
    {
        import bc.string.format : getFormatSize, nogcFormatTo;

        size_t total;
        // calculate total size needed so we don't have to reallocate
        static foreach (a; args) total += getFormatSize(a);

        // and format arguments to RCString
        auto ret = StringImpl(total);
        static foreach (a; args) ret.nogcFormatTo(a);
        return ret;
    }

    alias data this;

    /**
     * Access internal string including the reserved block if any.
     */
    @property inout(C)[] data() pure inout scope return
    {
        if (!length) return null;

        static if (!rc) {
            if (len + Z <= STACK_LEN) return stackBuf[0..len];
        }

        assert(pay.buf);
        auto ret = pay.buf[0..pay.len];
        return ret;
    }

    static if (zero)
    {
        /// Pointer to string data that can be directly used in a C functions expecting '\0' terminal char.
        @property inout(C*) ptr() pure inout scope return
        {
            if (!length) return null;
            static  if (!rc) {
                if (len + Z <= STACK_LEN) return &stackBuf[0];
            }
            auto ret = &pay.buf[0];
            return ret;
        }
    }

    /// Slicing support for the internal buffer data
    @property inout(C)[] opSlice() pure inout scope return
    {
        return this.data;
    }

    /// ditto
    @property inout(C)[] opSlice(size_t start, size_t end) pure inout @trusted return scope
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

    /// Managed string length
    @property size_t length() pure const
    {
        static if (rc)
            return pay ? pay.len : 0;
        else
            return len;
    }

    /// Returns: capacity that can be used without reallocation
    size_t capacity() pure const
    {
        static if (rc)
            return pay ? (pay.buf.length - pay.len - Z) : 0;
        else
            return (buf ? buf.length : STACK_LEN) - pay.len - Z;
    }

    /**
     * Reserves space for requested number of characters that also increments string length.
     * This can be used for example in cases when we need to fill slice of string with some known length data.
     * To return reserved data, use `dropBack`.
     */
    size_t reserve(size_t sz)
    {
        ensureAvail(sz);
        pay.len += sz;
        return sz;
    }

    /**
     * Drops defined amount of characters from the back.
     */
    void dropBack(size_t sz)
    {
        assert(length >= sz, "Not enough data");
        if (!sz) return;

        static if (!rc)
        {
            if (len + Z > STACK_LEN && len + Z - sz <= STACK_LEN)
            {
                // switch from heap buffer back to stack one
                len -= sz;
                stackBuf[0..len] = buf[0..len];
                static if (zero) stackBuf[len] = 0;
                return;
            }
        }
        pay.len -= sz;
        static if (zero) pay.buf[pay.len] = 0;
    }

    /**
     * Clears content of the data, but keeps internal buffer as is so it can be used to build another string.
     */
    void clear() pure
    {
        static if (rc) {
            if (pay) pay.len = 0;
        }
        else len = 0;
    }

    alias opOpAssign(string op : "~") = put;

    void opAssign(S)(auto ref scope S str)
    if (isAcceptableString!S || is(Unqual!S == C))
    {
        clear();
        put(str);
    }

    void put(in C val) pure
    {
        static if (!rc)
        {
            if (len + 1 + Z <= STACK_LEN)
            {
                stackBuf[len++] = val;
                static if (zero) stackBuf[len] = 0;
                return;
            }
        }
        ensureAvail(1);
        pay.buf[pay.len++] = val;
        static if (zero) pay.buf[pay.len] = 0;
    }

    void put(S)(auto ref scope S str) if (isAcceptableString!S)
    {
        alias CF = Unqual!(ElementEncodingType!S);

        static if (C.sizeof == CF.sizeof && is(typeof(pay.buf[0 .. str.length] = str[])))
        {
            static if (!rc)
            {
                if (len + str.length + Z <= STACK_LEN)
                {
                    stackBuf[len .. len + str.length] = str[];
                    len += str.length;
                    static if (zero) stackBuf[len] = 0;
                    return;
                }
            }

            ensureAvail(str.length);
            pay.buf[pay.len .. pay.len + str.length] = str[];
            pay.len += str.length;
            static if (zero) pay.buf[pay.len] = 0;
        }
        else
        {
            // copy range

            static if (!rc) size_t nlen = pay.len;
            static if (hasLength!S) {
                ensureAvail(str.length);
                static if (!rc) nlen += str.length;
            }
            import bc.internal.utf : byUTF;
            static if (isSomeString!S)
                auto r = cast(const(CF)[])str;  // because inout(CF) causes problems with byUTF
            else
                alias r = str;

            foreach (ch; r.byUTF!(Unqual!C))
            {
                static if (!hasLength!S || !is(C == CF))
                {
                    ensureAvail(1);
                    static if (!rc) {
                        static if (!hasLength!S) nlen++;
                        else {
                            if (pay.len == nlen) nlen++;
                        }
                    }
                }
                static if (!rc)
                {
                    if (nlen + Z + 1 <= STACK_LEN) // we can still use stack buffer
                    {
                        stackBuf[len++] = ch;
                        continue;
                    }
                }
                pay.buf[pay.len++] = ch;
            }
            static if (zero) pay.buf[pay.len] = 0;
            static if (!rc) assert(nlen == pay.len);
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
                immutable l = max(sz+Z, 64); // allocates at leas 64B
                pay.buf = () @trusted { return (cast(C*)enforceMalloc(l * C.sizeof))[0..l]; }();
                return;
            }

            if (pay.len + sz + Z <= pay.buf.length) return; // we can fit in what we've already allocated
        }
        else
        {
            if (len + sz + Z <= STACK_LEN) return; // still fits to stack buffer
            if (buf is null)
            {
                immutable l = max(len + sz + Z, STACK_LEN + 64); // allocates at leas 64B over
                buf = () @trusted { return (cast(C*)enforceMalloc(l * C.sizeof))[0..l]; }();
                buf[0..len] = stackBuf[0..len]; // copy data from stack buffer,  we'll use heap allocated one from now
                return;
            }
            if (len + Z <= STACK_LEN)
            {
                // some buffer is already preallocated, but we're still on stackBuffer and need to move to heap allocated one
                assert(buf.length > STACK_LEN);
                buf[0..len] = stackBuf[0..len]; // copy current data from the stack
            }

            if (len + sz + Z <= buf.length) return; // we can fit in what we've already allocated
        }

        // reallocate buffer
        // Note: new length calculation taken from std.array.appenderNewCapacity
        immutable ulong mult = 100 + (1000UL) / (bsr((pay.len + sz + Z)) + 1);
        immutable l = cast(size_t)(((pay.len + sz + Z) * min(mult, 200) + 99) / 100);
        // debug printf("realloc %lu -> %lu\n", pay.len, l);
        pay.buf = () @trusted { return (cast(C*)enforceRealloc(pay.buf.ptr, l * C.sizeof))[0..l]; }();
    }
}

auto rcString(C = char, S)(auto ref S str)
{
    StringImpl!(C, RC.yes, Zero.no) ret;
    ret.put(str);
    return ret;
}

@("RCString")
@system @nogc unittest
{
    import bc.internal.utf : byCodeUnit;
    import std.algorithm : filter;

    RCStringZ s;
    s ~= "fo";
    assert(s.pay.len == 2);
    assert(s.pay.buf.length >= 3);

    s ~= 'o';
    assert(s.pay.len == 3);
    assert(s.pay.buf.length >= 4);

    s ~= "bar";
    assert(s.pay.len == 6);
    assert(s.pay.buf.length >= 7);
    assert(s == "foobar");

    s ~= "baz".byCodeUnit.filter!(a => a == 'z');
    assert(s.length == "foobarz".length);
    assert(s.data == "foobarz");
    assert(s == "foobarz");
    assert(s.ptr == &s.data[0]);
    assert((s.ptr["foobarz".length]) == 0);
}

@("RCString.from")
@nogc @safe unittest
{
    {
        auto str = RCString.from("foo", 42, "bar");
        assert(str == "foo42bar");
    }

    {
        auto str = RCWString.from("foo");
        assert(str == "foo"w);
    }
}

version (D_Exceptions)
{
    @("RCString with Nullable")
    @nogc @safe unittest
    {
        import std.typecons : Nullable;
        Nullable!RCString sn = RCString("foo");
    }
}

@("rcString")
@nogc @safe unittest
{
    auto str = "foo".rcString();
    assert(str == "foo");
}

@("String")
@nogc @safe unittest
{
    auto s = String("Hello");
    assert(s.capacity == String.stackBuf.length - 5);
    assert(s[] == "Hello");
    s ~= " String";
    assert(s[] == "Hello String");
    auto s2 = s.clone();
    assert(s[] == s2[]);
    () @trusted { assert(s.ptr != s2.ptr); }();

    auto s3 = s.move();
    assert(s.buf is null);
    assert(s.len == 0);
    assert(s3 == "Hello String");
}

@("String - put static array")
@nogc @safe unittest
{
    String s;
    immutable(char)[3] foo = "foo";
    s ~= foo;
    assert(s == "foo");
}

@("String stack to heap")
@nogc @safe unittest
{
    import std.algorithm : each;
    import std.range : repeat;

    StringZ s;
    'a'.repeat(s.stackBuf.length-1).each!(c => s.put(c));
    assert(s.length == s.stackBuf.length-1);
    assert(s.stackBuf[$-2] == 'a');
    assert(s.stackBuf[$-1] == '\0');
    assert(s.buf is null);
    assert(&s.data[0] == &s.stackBuf[0]);
    s ~= 'b';
    assert(s.stackBuf[$-1] == '\0'); // doesn't change on stack to heap switch
    assert(s.buf !is null);
    assert(&s.data[0] == &s.buf[0]);
    assert(s.buf[s.stackBuf.length-1] == 'b');
    s ~= "foo";

    s.clear();
    s ~= 'c';
    assert(&s.data[0] == &s.stackBuf[0]); // back to stack usage
    assert(s.buf !is null); // but heap buffer is still there
    'd'.repeat(s.stackBuf.length).each!(c => s.put(c));
    assert(&s.data[0] == &s.buf[0]);
    assert(s.length == 1 + s.stackBuf.length);
    assert(s.buf[1 + s.stackBuf.length] == '\0');
}

@("String reserve")
@nogc @safe unittest
{
    String buf;
    assert(buf.length == 0);
    assert(buf.capacity == buf.stackBuf.length);
    buf.reserve(64);
    assert(buf.length == 64);
    assert(buf.buf is null);
    buf[][0..3] = "foo";
    buf.dropBack(61);
    assert(buf[] == "foo");
    buf.reserve(buf.stackBuf.length);
    assert(buf.buf !is null);
    assert(buf.buf[0..3] == "foo");
    buf.buf[0..3] = "bar";
    buf.dropBack(buf.stackBuf.length);
    assert(buf.buf !is null); // left allocated for reuse
    assert(buf.stackBuf[0..3] == "bar"); // copy from heap
}

private C[] trustedRealloc(C)(scope C[] buf, size_t strLength, bool bufIsOnStack)
    @trusted @nogc pure nothrow
{
    pragma(inline, false);  // because it's rarely called

    import bc.core.memory : enforceMalloc, enforceRealloc;

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

/// Strips leading whitespace ('\t', '\n', '\r', ' ')
S stripLeft(S)(S str)
{
    pragma(inline, true);
    /// All chars except for whitespace ('\t', '\n', '\r', ' ')
    enum AllExceptWhitespaceRanges = "\0\10\13\14\16\37\41\377";

    size_t rpos;
    immutable rs = parseToken!(AllExceptWhitespaceRanges, '"')(str, rpos);
    if(rs == -1) return str[$..$]; // Only whitespace string, return empty range.
    else return str[rpos..$];
}

@("stripLeft")
unittest
{
    assert(stripLeft("\t\n\r foobar\t\n\r ") == "foobar\t\n\r ");
    assert(stripLeft("\t\n\r\t\n\r ") == "");
}

bool startsWith(S, char[] chars)(S str)
{
    enum validCharMap = buildValidCharMap(chars, true);
    return validCharMap[str[0]];
}

@("startsWith")
unittest
{
    assert(startsWith!(string, ['+', '-'])("-42"));
    assert(startsWith!(string, ['+', '-'])("+42"));
    assert(!startsWith!(string, ['+', '-'])("42"));
}

/// Builds a char-map ranges string (pairs of [from, to]) covering exactly the given characters.
auto buildCharMapRanges(string chars)()
{
    char[chars.length * 2] ranges;
    static foreach (i, ch; chars)
    {
        ranges[i * 2] = chars[i];
        ranges[(i * 2) + 1] = chars[i];
    }
    return ranges;
}

/// Strips leading characters belonging to the `chars` set.
S stripLeft(string chars, S)(S str)
{
    enum ranges = buildCharMapRanges!(chars)();
    enum charMap = buildValidCharMap(ranges, true);

    foreach (i, c; str)
        if (!charMap[c]) return str[i..$];
    return str[0..0];
}

@("stripLeft - custom")
@safe unittest
{
    assert(stripLeft!"ab"("abfoob") == "foob");
}

/// Strips trailing characters belonging to the `chars` set (whitespace by default).
S stripRight(string chars = "\t\n\r ", S)(S str)
{
    pragma(inline, true);
    enum ranges = buildCharMapRanges!(chars)();
    enum charMap = buildValidCharMap(ranges, true);

    foreach_reverse (i, c; str)
        if (!charMap[c]) return str[0..i+1];
    return str[0..0];
}

@("stripRight")
@safe unittest
{
    assert(stripRight("\t\n\r foobar\t\n\r ") == "\t\n\r foobar");
    assert(stripRight("\t\n\r\t\n\r ") == "");
    assert(stripRight!"ab"("abfooab") == "abfoo");
}

/// Strips leading and trailing characters belonging to the `chars` set (whitespace by default).
S strip(string chars = "\t\n\r ", S)(S str)
{
    pragma(inline, true);
    return str.stripLeft!chars().stripRight!chars();
}

@("strip")
@safe unittest
{
    assert(strip("\t\n\r foobar\t\n\r ") == "foobar");
    assert(strip("\t\n\r\t\n\r ") == "");
}

/// Returns whether all characters in `str` are already in lower-case (ASCII).
bool isLower(const(char)[] str) @safe pure
{
    import bc.string.ascii : toLower;
    foreach (c; str)
        if (c.toLower != c) return false;
    return true;
}

@safe unittest
{
    assert(isLower("foobar"));
    assert(!isLower("fooBar"));
}

/// Returns the index of the first occurrence of `needle` in `str`, or -1.
ptrdiff_t indexOf(const(char)[] str, const(char)[] needle) @safe pure nothrow @nogc
in (needle.length, "No needle provided")
{
    import core.stdc.string : memcmp;
    size_t idx;
    while (idx + needle.length <= str.length)
    {
        if (() @trusted { return memcmp(&str[idx], needle.ptr, needle.length); }() == 0)
            return idx;
        idx++;
    }
    return -1;
}

/// Returns the index of the last occurrence of `c` in `str`, or -1.
ptrdiff_t lastIndexOf(const(char)[] str, char c) @safe pure nothrow @nogc
{
    if (_expect(!str.length, false)) return -1;
    for (ptrdiff_t i = str.length - 1; i >= 0; --i)
        if (str[i] == c) return i;
    return -1;
}

@("indexOf")
@safe unittest
{
    assert("foobar".indexOf("bar") == 3);
    assert("foobar".indexOf("baz") == -1);
    assert("foo.bar.baz".lastIndexOf('.') == 7);
    assert("foobar".lastIndexOf('.') == -1);
}

/// Returns whether `str` ends with `postfix`.
bool endsWith(const(char)[] str, const(char)[] postfix) @safe pure nothrow @nogc
in (postfix.length, "Empty postfix")
{
    import core.stdc.string : memcmp;
    pragma(inline, true);
    if (_expect(str.length < postfix.length, false)) return false;
    return () @trusted { return memcmp(&str[$-postfix.length], postfix.ptr, postfix.length) == 0; }();
}

/// Case insensitive variant; `postfix` must already be in lower-case.
bool endsWith(string postfix)(const(char)[] str)
in (postfix.length, "Empty postfix")
{
    if (_expect(str.length < postfix.length, false)) return false;
    return streqi!postfix(str[$-postfix.length..$]);
}

@("endsWith")
@safe unittest
{
    assert("foobar".endsWith("bar"));
    assert(!"foo".endsWith("bar"));
    assert("foobar".endsWith!"bar");
    assert("fooBAR".endsWith!"bar");
}

/**
 * Specialized case insensitive equality comparison against a compile-time constant string.
 * The template parameter string must already be in lower-case (saves needless work).
 */
bool streqi(string a)(const(char)[] b) @trusted
{
    pragma(inline, true);
    static assert(a.isLower, "Template parameter must be already in lower case");
    import bc.string.ascii : toLower;
    if (_expect(a.length != b.length, false)) return false;

    enum maxStackLen = 512;
    static if (a.length <= maxStackLen)
    {
        char[a.length] low = void;
        for (int i = 0; i < a.length; ++i) low[i] = b[i].toLower;
        return low[] == a;
    }
    else
    {
        size_t i;
        while (a.length - i >= 8)
        {
            static foreach (_; 0..8)
                if (a[i] != b[i++].toLower) return false;
        }
        for (; i < a.length; ++i)
            if (a[i] != b[i].toLower) return false;
        return true;
    }
}

@("streqi")
@safe unittest
{
    assert(streqi!"foobar"("FooBar"));
    assert(!streqi!"foobar"("FooBaz"));
    assert(!streqi!"foo"("foobar"));
}

/**
 * Replaces all occurrences of `oldPart` with `newPart` in `text`.
 * Note: the result is a slice into a shared static buffer, valid only until the next call.
 */
const(char)[] replace(const(char)[] text, const(char)[] oldPart, const(char)[] newPart) @safe nothrow @nogc
{
    static String buf;
    buf.clear();

    size_t pos;
    while (true)
    {
        auto idx = text[pos..$].indexOf(oldPart);
        if (idx == -1)
        {
            buf ~= text[pos..$];
            break;
        }
        buf ~= text[pos..pos+idx];
        buf ~= newPart;
        pos += idx + oldPart.length;
    }
    return buf.data();
}

@("replace")
@safe unittest
{
    assert(":::::".replace(":", "-") == "-----");
    assert("a:b:c".replace(":", "") == "abc");
}

/// Simple fixed size string appender over a caller-provided buffer.
struct FixedString
{
    nothrow @nogc @safe pure:

    this(return scope char[] buf) { this.buf = buf; }

    void put(const(char)[] str) scope
    in (buf, "Not initialized")
    {
        if (_expect(!str.length, false)) return;
        assert(len + str.length <= buf.length, "Data won't fit");
        import core.stdc.string : memcpy;
        () @trusted { memcpy(&buf[len], str.ptr, str.length); }();
        len += str.length;
    }

    void put(char c) scope
    in (buf, "Not initialized")
    {
        assert(len < buf.length, "Data won't fit");
        buf[len++] = c;
    }

    void clear() scope { len = 0; }

    size_t length() const scope { return len; }

    /// Slicing support for the internal buffer data
    inout(char)[] opSlice() pure inout return scope { return buf[0..len]; }

    /// ditto
    inout(char)[] opSlice(size_t start, size_t end) pure inout return scope
    {
        if (start > length || end > length) assert(0, "Index out of bounds");
        if (start > end) assert(0, "Invalid slice indexes");
        return buf[start .. end];
    }

    private:
    char[] buf;
    size_t len;
}

@("FixedString")
@safe unittest
{
    char[16] b;
    auto fs = FixedString(b[]);
    fs.put("foo");
    fs.put('-');
    fs.put("bar");
    assert(fs[] == "foo-bar");
    assert(fs.length == 7);
}

/**
 * Alternative implementation of `std.string.outdent` that differs in:
 *
 *   * meant for dedent string literals in CT
 *   * if first line is not indented, other lines are dedented still (std.string.outdent returns original text in that case)
 *   * empty lines at the text start are removed
 */
template dedent(alias str)
{
    static S getLine(S)(S str)
    {
        if (!str.length) return null;
        for (size_t i = 0; i < str.length; ++i)
        {
            if (str[i] == '\r')
            {
                if (i+1 < str.length && str[i+1] == '\n')
                    return str[0..i+2];
            }
            if (str[i] == '\n') return str[0..i+1];
        }
        return str;
    }

    // strip line whitespace but keep newline characters
    static S stripWS(S)(S str)
    {
        if (!str.length) return null;
        for (size_t i = 0; i < str.length; ++i)
        {
            if (str[i] <= ' ' && str[i] != '\r' && str[i] != '\n') continue;
            return str[i..$];
        }
        return null;
    }

    template shortestIndent(alias str, size_t prev = size_t.max)
    {
        enum line = getLine(str);
        enum stripped = stripWS(line);
        static if (line.length == 0) enum shortestIndent = prev;
        else static if (line.length == stripped.length) enum shortestIndent = 0;
        else
        {
            enum cur = prev > line.length - stripped.length ? line.length - stripped.length : prev;
            enum next = shortestIndent!(str[line.length..$], cur);
            enum shortestIndent = cur > next ? next : cur;
        }
    }

    template dedentNext(alias str, size_t indent)
    {
        enum ln = getLine(str);
        static if (!ln.length)
            enum dedentNext = null;
        else static if (ln.length < indent)
            enum dedentNext = ln ~ dedentNext!(str[ln.length..$], indent);
        else
            enum dedentNext = ln[indent..$] ~ dedentNext!(str[ln.length..$], indent);
    }

    enum line = getLine(str);
    enum stripped = stripWS(line);

    static if (!line.length) enum dedent = null;
    else static if (
            (stripped.length == 1 && stripped[0] == '\n')
            || (stripped.length == 2 && stripped[0] == '\r' && stripped[1] == '\n'))
        enum dedent = dedent!(str[line.length..$]); // drop first empty lines
    else
    {
        // ignore no indentation of the first line
        enum shortest = shortestIndent!(
            str[line.length..$],
            stripped.length == line.length ? size_t.max : (line.length - stripped.length));

        static if (shortest == 0)
            enum dedent = str; // no indent used
        else
            enum dedent = stripped ~ dedentNext!(str[line.length..$], shortest);
    }
}

@("dedent")
unittest
{
    // with empty first line
    {
        enum str1 = `
                DELETE FROM elements.element
                WHERE id=ANY($1) AND type_id IN (
                    SELECT id FROM elements.element_type WHERE owner=$2
                )`;

        enum str2 =
                    "DELETE FROM elements.element\n" ~
                    "WHERE id=ANY($1) AND type_id IN (\n" ~
                    "    SELECT id FROM elements.element_type WHERE owner=$2\n" ~
                    ")";

        static assert(dedent!str1 == str2);
    }

    // with not indented first line
    {
        enum str1 = `DELETE FROM elements.element
                WHERE id=ANY($1) AND type_id IN (
                    SELECT id FROM elements.element_type WHERE owner=$2
                )`;

        enum str2 = "DELETE FROM elements.element\n" ~
                    "WHERE id=ANY($1) AND type_id IN (\n" ~
                    "    SELECT id FROM elements.element_type WHERE owner=$2\n" ~
                    ")";

        static assert(dedent!str1 == str2);
    }

    // test that we didn't touch number of lines
    {
        static assert(dedent!`
            2
            3
            ` == "2\n3\n"); // first line is dropped, last newline is kept
    }

    // test we don't dedent when some line is not indented
    {
        enum str = `aa
            bb
cc`;
        assert(dedent!str == str);
    }

    // test that we don't touch space after last line text
    {
        assert(dedent!"  foo " == "foo ");
        assert(dedent!`foo
            bar ` == "foo\nbar ");
    }
}

/**
 * Builds char map from the provided ranges.
 *
 * Params:
 *      ranges = ranges of ascii characters.
 *      valid = wheteher range characters are valid or not.
 *              For example:
 *              buildValidCharMap("\0/:\xff", false)   means that only characters 0-9 would have true in the generated map.
 *              buildValidCharMap("\0/:\xff", true)    means that all characters except 0-9 would have true in the generated map.
 *
 * Returns: generated table
 */
bool[256] buildValidCharMap(S)(S ranges, bool valid = false)
{
    assert(ranges.length % 2 == 0, "Uneven ranges");
    bool[256] res = valid ? false : true;

    for (int i=0; i < ranges.length; i+=2)
        for (int j=ranges[i]; j <= ranges[i+1]; ++j)
            res[j] = valid ? true : false;
    return res;
}

///
@("buildValidCharMap")
@safe unittest
{
    string ranges = "\0 \"\"(),,//:@[]{{}}\x7f\xff";
    assert(buildValidCharMap(ranges, false) ==
        cast(bool[])[
            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
            0,1,0,1,1,1,1,1,0,0,1,1,0,1,1,0,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,
            0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,1,1,
            1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,0,1,0,
            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        ]);

    assert(buildValidCharMap(ranges, true) ==
        cast(bool[])[
            1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
            1,0,1,0,0,0,0,0,1,1,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,
            1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,
            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,1,
            1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
            1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
            1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
            1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
        ]);
}

/*
 * Advances index over the token to the next character while checking for valid characters.
 * On success, buffer index is left on the next character.
 *
 * Params:
 *   - ranges = ranges of characters to stop on
 *   - next  = next character/s to stop on (must be present in the provided ranges too)
 *   - sseRanges =
 *         as SSE optimized path is limited to 8 pairs, here one can provide merged ranges for a fast
 *         SSE path that would be precised with `ranges`. Otherwise `ranges` is used for SSE path too.
 *
 * Returns:
 *     * 0 on success
 *     * -1 when token hasn't been found (ie not enough data in the buffer)
 *     * -2 when character from invalid ranges was found but not matching one of next characters (ie invalid token)
 */
int parseToken(string ranges, alias next, string sseRanges = null, C)(const(C)[] buffer, ref size_t i) pure
    if (is(C == ubyte) || is(C == char))
{
    version (DigitalMars) {
        static if (__VERSION__ >= 2094) pragma(inline, true); // older compilers can't inline this
    } else pragma(inline, true);

    immutable charMap = parseTokenCharMap!(ranges)();

    static if (LDC_with_SSE42)
    {
        // CT function to prepare input for SIMD vector enum
        static byte[16] padRanges()(string ranges)
        {
            byte[16] res;
            // res[0..ranges.length] = cast(byte[])ranges[]; - broken on macOS betterC tests
            foreach (i, c; ranges) res[i] = cast(byte)c;
            return res;
        }

        static if (sseRanges) alias usedRng = sseRanges;
        else alias usedRng = ranges;
        static assert(usedRng.length <= 16, "Ranges must be at most 16 characters long");
        static assert(usedRng.length % 2 == 0, "Ranges must have even number of characters");
        enum rangesSize = usedRng.length;
        enum byte16 rngE = padRanges(usedRng);

        if (_expect(buffer.length - i >= 16, true))
        {
            size_t left = (buffer.length - i) & ~15; // round down to multiple of 16
            byte16 ranges16 = rngE;

            do
            {
                byte16 b16 = () @trusted { return cast(byte16)_mm_loadu_si128(cast(__m128i*)&buffer[i]); }();
                immutable r = _mm_cmpestri(
                    ranges16, rangesSize,
                    b16, 16,
                    _SIDD_LEAST_SIGNIFICANT | _SIDD_CMP_RANGES | _SIDD_UBYTE_OPS
                );

                if (r != 16)
                {
                    i += r;
                    goto FOUND;
                }
                i += 16;
                left -= 16;
            }
            while (_expect(left != 0, true));
        }
    }
    else
    {
        // faster unrolled loop to iterate over 8 characters
        loop: while (_expect(buffer.length - i >= 8, true))
        {
            static foreach (_; 0..8)
            {
                if (_expect(!charMap[buffer[i]], false)) goto FOUND;
                ++i;
            }
        }
    }

    // handle the rest
    if (_expect(i >= buffer.length, false)) return -1;

    FOUND:
    while (true)
    {
        static if (is(typeof(next) == char)) {
            static assert(!charMap[next], "Next character is not in ranges");
            if (buffer[i] == next) return 0;
        } else {
            static assert(next.length > 0, "Next character not provided");
            static foreach (c; next) {
                static assert(!charMap[c], "Next character is not in ranges");
                if (buffer[i] == c) return 0;
            }
        }
        if (_expect(!charMap[buffer[i]], false)) return -2;
        if (_expect(++i == buffer.length, false)) return -1;
    }
}

///
@("parseToken")
@safe unittest
{
    size_t idx;
    string buf = "foo\nbar";
    auto ret = parseToken!("\0\037\177\377", "\r\n")(buf, idx);
    assert(ret == 0); // no error
    assert(idx == 3); // index of newline character

    idx = 0;
    ret = parseToken!("\0\037\177\377", "\r\n")(buf[0..3], idx);
    assert(ret == -1); // not enough data to find next character
    assert(idx == 3);

    idx = 0;
    buf = "foo\t\nbar";
    ret = parseToken!("\0\037\177\377", "\r\n")(buf, idx);
    assert(ret == -2); // invalid character '\t' found in token
    assert(idx == 3); // invalid character on index 3
}

private immutable(bool[256]) parseTokenCharMap(string invalidRanges)() {
    static immutable charMap = buildValidCharMap(invalidRanges);
    return charMap;
}
