/**
 * @nogc formatting utilities
 *
 * Inspired by: https://github.com/weka-io/mecca/blob/master/src/mecca/lib/string.d
 *
 * Sink Types:
 * various functions in this module use "sinks" which are buffers or objects that get filled with
 * the formatting data while the format functions are running. The following sink types are
 * supported to be passed into these arguments:
 * - Arrays (`isArray!S && is(ForeachType!S : char))`)
 * - $(LREF NullSink)
 * - Object with `put(const(char)[])` and `put(char)` functions
 *
 * Passing in arrays will make the sink `@nogc pure nothrow @safe` as everything will be written
 * into that memory. Passing in arrays that are too short to hold all the data will trigger a
 * `RangeError` or terminate the program in betterC.
 *
 * Passing in a $(LREF NullSink) instance will not allocate any memory and just count the bytes that
 * will be allocated.
 *
 * Otherwise any type that contains a `put` method that can be called both with `const(char)[]` and
 * with `char` arguments can be used.
 */
module bc.string.format;

import bc.core.intrinsics;
import bc.core.system.backtrace;
import bc.core.traits;
import bc.string.string;
import std.algorithm : among;
import std.datetime.date : TimeOfDay;
import std.traits :
    EnumMembers, FieldNameTuple, ForeachType, hasMember,
    isArray, isPointer, isSigned, isSomeChar, isStaticArray,
    PointerTarget, Unqual;
import std.range : ElementEncodingType, isForwardRange, isInputRange;
import std.typecons : Flag, Tuple, isTuple;

version (D_BetterC) {}
else
{
    import core.time : Duration;
    import std.datetime.systime : SysTime;
    import std.uuid : UUID;
}

private template isUUID(T)
{
    version (D_BetterC) enum isUUID = false;
    else enum isUUID = is(T == UUID);
}

private template isSysTime(T)
{
    version (D_BetterC) enum isSysTime = false;
    else enum isSysTime = is(T == SysTime);
}

private template isDuration(T)
{
    version (D_BetterC) enum isDuration = false;
    else enum isDuration = is(T == Duration);
}

private template isTraceInfo(T)
{
    version (D_BetterC) enum isTraceInfo = false;
    else version (linux) enum isTraceInfo = is(T == TraceInfo);
    else enum isTraceInfo = false;
}

/**
 * Formats values to with fmt template into provided sink.
 * Note: it supports only a basic subset of format type specifiers, main usage is for nogc logging
 * and error messages formatting. But more cases can be added as needed.
 *
 * WARN: %s accepts pointer to some char assuming it's a zero terminated string
 *
 * Params:
 *     fmt  = The format string, much like in std.format
 *     sink = The sink where the full string should be written to, see section "Sink Types"
 *     args = The arguments to fill the format string with
 *
 * Returns: the length of the formatted string.
 */
size_t nogcFormatTo(string fmt = "%s", S, ARGS...)(ref S sink, auto ref ARGS args)
{
    // TODO: not pure because of float formatter
    alias sfmt = splitFmt!fmt;
    static assert (sfmt.numFormatters == ARGS.length, "Expected " ~ sfmt.numFormatters.stringof ~
        " arguments, got " ~ ARGS.length.stringof);

    mixin SinkWriter!S;

    foreach (tok; sfmt.tokens) {
        // pragma(msg, "tok: ", tok);
        static if (is(typeof(tok) == string))
        {
            static if (tok.length > 0) {
                write(tok);
            }
        }
        else static if (is(typeof(tok) == ArrFmtSpec))
        {
            enum j = tok.idx;
            alias Typ = Unqual!(ARGS[j]);
            static assert(
                __traits(compiles, ForeachType!Typ), "Expected foreach type range instead of " ~ Typ.stringof);
            static assert(
                !is(S == NullSink) || isArray!Typ || isForwardRange!Typ,
                "Don't determine format output length with range argument " ~ Typ.stringof ~ " it'd be consumed.");
            static if (!is(S == NullSink) && !isArray!Typ && !isForwardRange!Typ)
                pragma(msg, "WARN: Argument of type " ~ Typ.stringof ~ " would be consumed during format");

            static if (tok.del.length) bool first = true;
            static if (!isArray!Typ && isForwardRange!Typ) auto val = args[j].save();
            else auto val = args[j];
            foreach (ref e; val)
            {
                static if (tok.del.length) {
                    if (_expect(!first, true)) write(tok.del);
                    else first = false;
                }
                advance(s.nogcFormatTo!(tok.fmt)(e));
            }
        }
        else static if (is(typeof(tok) == FmtSpec)) {
            enum j = tok.idx;
            enum f = tok.type;

            alias Typ = Unqual!(ARGS[j]);
            auto val = args[j];

            static if (isStdNullable!Typ) {
                if (val.isNull) write("null");
                else advance(s.nogcFormatTo!"%s"(val.get));
            }
            else static if (f == FMT.STR) {
                static if ((isArray!Typ && is(Unqual!(ForeachType!Typ) == char)))
                    write(val[]);
                else static if (isInputRange!Typ && isSomeChar!(Unqual!(ElementEncodingType!Typ))) {
                    import bc.internal.utf : byUTF;
                    foreach (c; val.byUTF!char) write(c);
                }
                else static if (is(Typ == bool))
                    write(val ? "true" : "false");
                else static if (is(Typ == enum)) {
                    auto tmp = enumToStr(val);
                    if (_expect(tmp is null, false)) advance(s.nogcFormatTo!"%s(%d)"(Typ.stringof, val));
                    else write(tmp);
                } else static if (isUUID!Typ) advance(s.formatUUID(val));
                else static if (isSysTime!Typ) advance(s.formatSysTime(val));
                else static if (is(Typ == TimeOfDay))
                    advance(s.nogcFormatTo!"%02d:%02d:%02d"(val.hour, val.minute, val.second));
                else static if (isDuration!Typ) advance(s.formatDuration(val));
                else static if (isArray!Typ || isInputRange!Typ) {
                    import std.range : empty;
                    if (!val.empty) advance(s.nogcFormatTo!"[%(%s%|, %)]"(val));
                    else write("[]");
                }
                else static if (isPointer!Typ) {
                    static if (is(typeof(*Typ)) && isSomeChar!(typeof(*Typ))) {
                        // NOTE: not safe, we can only trust that the provided char pointer is really stringz
                        size_t i;
                        while (val[i] != '\0') ++i;
                        if (i) write(val[0..i]);
                    }
                    else advance(s.formatPtr(val));
                }
                else static if (is(Typ == char)) write(val);
                else static if (isSomeChar!Typ) {
                    import std.range : only;
                    import bc.internal.utf : byUTF;

                    foreach (c; val.only.byUTF!char) write(c);
                }
                else static if (is(Typ : ulong)) advance(s.formatDecimal(val));
                else static if (isTuple!Typ) {
                    write("Tuple(");
                    foreach (i, _; Typ.Types)
                    {
                        static if (Typ.fieldNames[i] == "") enum prefix = (i == 0 ? "" : ", ");
                        else enum prefix = (i == 0 ? "" : ", ") ~ Typ.fieldNames[i] ~ "=";
                        write(prefix);
                        advance(s.nogcFormatTo!"%s"(val[i]));
                    }
                    write(")");
                }
                else static if (is(Typ : Throwable)) {
                    auto obj = cast(Object)val;
                    static if (__traits(compiles, TraceInfo(val))) {
                        advance(s.nogcFormatTo!"%s@%s(%d): %s\n----------------\n%s"(
                            typeid(obj).name, val.file, val.line, val.msg, TraceInfo(val)));
                    }
                    else
                        advance(s.nogcFormatTo!"%s@%s(%d): %s"(
                            typeid(obj).name, val.file, val.line, val.msg));
                }
                else static if (isTraceInfo!Typ) {
                    auto sw = sinkWrap(s);
                    val.dumpTo(sw);
                    advance(sw.totalLen);
                }
                else static if (is(typeof(val[])))
                    advance(s.nogcFormatTo!"%s"(val[])); // sliceable values
                else static if (is(Typ == struct)) {
                    static if (__traits(compiles, (v) @nogc {auto sw = sinkWrap(s); v.toString(sw); }(val))) {
                        // we can use custom defined toString
                        auto sw = sinkWrap(s);
                        val.toString(sw);
                        advance(sw.totalLen);
                    } else {
                        static if (hasMember!(Typ, "toString"))
                            pragma(msg, Typ.stringof ~ " has toString defined, but can't be used with nogcFormatter");
                        {
                            enum Prefix = Typ.stringof ~ "(";
                            write(Prefix);
                        }
                        alias Names = FieldNameTuple!Typ;
                        foreach(i, field; val.tupleof) {
                            enum string Name = Names[i];
                            enum Prefix = (i == 0 ? "" : ", ") ~ Name ~ "=";
                            write(Prefix);
                            advance(s.nogcFormatTo!"%s"(field));
                        }
                        write(")");
                    }
                } else static if (is(Typ : double)) advance(s.nogcFormatTo!"%g"(val));
                else static assert (false, "Unsupported value type for string format: " ~ Typ.stringof);
            }
            else static if (f == FMT.CHR) {
                static assert (is(Typ : char), "Requested char format, but provided: " ~ Typ.stringof);
                write((&val)[0..1]);
            }
            else static if (f == FMT.DEC) {
                static assert (is(Typ : ulong), "Requested decimal format, but provided: " ~ Typ.stringof);
                enum fs = formatSpec(f, tok.def);
                advance(s.formatDecimal!(fs.width, fs.fill)(val));
            }
            else static if (f == FMT.HEX || f == FMT.UHEX) {
                static assert (is(Typ : ulong) || isPointer!(Typ), "Requested hex format, but provided: " ~ Typ.stringof);
                enum u = f == FMT.HEX ? Upper.yes : Upper.no;
                enum fs = formatSpec(f, tok.def);
                static if (isPointer!(Typ)) {
                    import std.stdint : intptr_t;
                    advance(s.formatHex!(fs.width, fs.fill, u)(cast(intptr_t)val));
                }
                else advance(s.formatHex!(fs.width, fs.fill, u)(val));
            }
            else static if (f == FMT.PTR) {
                static assert (is(Typ : ulong) || isPointer!(Typ), "Requested pointer format, but provided: " ~ Typ.stringof);
                advance(s.formatPtr(val));
            }
            else static if (f == FMT.FLT) {
                static assert (is(Typ : double), "Requested float format, but provided: " ~ Typ.stringof);
                advance(s.formatFloat(val));
            }
        }
        else static assert(false);
    }

    return totalLen;
}

///
@("combined")
@safe @nogc unittest
{
    char[100] buf;
    ubyte[3] data = [1, 2, 3];
    immutable ret = nogcFormatTo!"hello %s %s %% world %d %x %p"(buf, data, "moshe", -567, 7, 7);
    assert(ret == 53);
    assert(buf[0..53] == "hello [1, 2, 3] moshe % world -567 7 0000000000000007");
}

/**
 * Same as `nogcFormatTo`, but it internally uses static malloc buffer to write formatted string to.
 * So be careful that next call replaces internal buffer data and previous result isn't valid anymore.
 */
const(char)[] nogcFormat(string fmt = "%s", ARGS...)(auto ref ARGS args)
{
    import bc.string.string : String;
    static String str;
    str.clear();
    nogcFormatTo!fmt(str, args);
    return cast(const(char)[])str.data;
}

///
@("formatters")
@safe unittest
{
    import bc.core.memory;
    import std.algorithm : filter;
    import std.range : chunks;

    assert(nogcFormat!"abcd abcd" == "abcd abcd");
    assert(nogcFormat!"123456789a" == "123456789a");
    version (D_NoBoundsChecks) {}
    else version (D_Exceptions)
    {
        () @trusted
        {
            import core.exception : RangeError;
            import std.exception : assertThrown;
            char[5] buf;
            assertThrown!RangeError(buf.nogcFormatTo!"123412341234");
        }();
    }

    // literal escape
    assert(nogcFormat!"123 %%" == "123 %");
    assert(nogcFormat!"%%%%" == "%%");

    // %d
    assert(nogcFormat!"%d"(1234) == "1234");
    assert(nogcFormat!"%4d"(42) == "  42");
    assert(nogcFormat!"%04d"(42) == "0042");
    assert(nogcFormat!"%04d"(-42) == "-042");
    assert(nogcFormat!"ab%dcd"(1234) == "ab1234cd");
    assert(nogcFormat!"ab%d%d"(1234, 56) == "ab123456");

    // %x
    assert(nogcFormat!"0x%x"(0x1234) == "0x1234");

    // %p
    assert(nogcFormat!("%p")(0x1234) == "0000000000001234");

    // %s
    assert(nogcFormat!"12345%s"("12345") == "1234512345");
    assert(nogcFormat!"12345%s"(12345) == "1234512345");
    enum Floop {XXX, YYY, ZZZ}
    assert(nogcFormat!"12345%s"(Floop.YYY) == "12345YYY");
    char[4] str = "foo\0";
    assert(() @trusted { return nogcFormat!"%s"(str.ptr); }() == "foo");

    version (D_BetterC) {}
    else
    {
        assert(nogcFormat!"%s"(
            UUID([138, 179, 6, 14, 44, 186, 79, 35, 183, 76, 181, 45, 179, 189, 251, 70]))
            == "8ab3060e-2cba-4f23-b74c-b52db3bdfb46");
    }

    // array format
    version (D_BetterC)
    {
        int[] arr = () @trusted { return (cast(int*)enforceMalloc(int.sizeof*10))[0..10]; }();
        foreach (i; 0..10) arr[i] = i;
        scope (exit) () @trusted { pureFree(arr.ptr); }();
    }
    else auto arr = [0,1,2,3,4,5,6,7,8,9];

    assert(nogcFormat!"foo %(%d %)"(arr[1..4]) == "foo 1 2 3");
    assert(nogcFormat!"foo %-(%d %)"(arr[1..4]) == "foo 1 2 3");
    assert(nogcFormat!"foo %(-%d-%|, %)"(arr[1..4]) == "foo -1-, -2-, -3-");
    assert(nogcFormat!"%(0x%02x %)"(arr[1..4]) == "0x01 0x02 0x03");
    assert(nogcFormat!"%(%(%d %)\n%)"(arr[1..$].chunks(3)) == "1 2 3\n4 5 6\n7 8 9");

    // range format
    auto r = arr.filter!(a => a < 5);
    assert(nogcFormat!"%s"(r) == "[0, 1, 2, 3, 4]");

    // Arg num
    assert(!__traits(compiles, nogcFormat!"abc"(5)));
    assert(!__traits(compiles, nogcFormat!"%d"()));
    assert(!__traits(compiles, nogcFormat!"%d a %d"(5)));

    // Format error
    assert(!__traits(compiles, nogcFormat!"%"()));
    assert(!__traits(compiles, nogcFormat!"abcd%d %"(15)));
    assert(!__traits(compiles, nogcFormat!"%$"(1)));
    assert(!__traits(compiles, nogcFormat!"%d"("hello")));
    assert(!__traits(compiles, nogcFormat!"%x"("hello")));

    assert(nogcFormat!"Hello %s"(5) == "Hello 5");

    struct Foo { int x, y; }
    assert(nogcFormat!("Hello %s")(Foo(1, 2)) == "Hello Foo(x=1, y=2)");

    version (D_BetterC)
    {
        struct Nullable(T) // can't be instanciated in betterC - fake just for the UT
        {
            T get() { return T.init; }
            bool isNull() { return true; }
            void nullify() {}
        }
    }
    else import std.typecons : Nullable;

    struct Msg { Nullable!string foo; }
    assert(nogcFormat!"%s"(Msg.init) == "Msg(foo=null)");

    RCString s = "abcd";
    assert(nogcFormat!"%s"(s) == "abcd");
}

///
@("tuple")
@safe unittest
{
    {
        alias T = Tuple!(int, "foo", bool);
        T t = T(42, true);
        assert(nogcFormat(t) == "Tuple(foo=42, true)");
    }

    {
        alias T = Tuple!(int, "foo", string, "bar", char, "baz");
        T t = T(42, "bar", 'z');
        assert(nogcFormat(t) == "Tuple(foo=42, bar=bar, baz=z)");
    }
}

///
@("custom format")
@safe unittest
{
    static struct Custom
    {
        int foo = 42;
        void toString(S)(ref S sink) const
        {
            sink.put("custom: ");
            sink.nogcFormatTo!"foo=%d"(foo);
        }
    }

    Custom c;
    assert(nogcFormat(c) == "custom: foo=42");
    assert(getFormatSize(c) == "custom: foo=42".length);
}

version (D_BetterC) {}
else version (linux)
{
    // Only Posix is supported ATM
    @("Exception stack trace format")
    @safe unittest
    {
        import std.algorithm : startsWith;
        static class TestException : Exception { this(string msg) nothrow { super(msg); } }
        static void fn() { throw new TestException("foo"); }

        try fn();
        catch (Exception ex)
        {
            import std.format : format;
            string std = () @trusted { return format!"Now how cool is that!: %s"(ex); }();
            (Exception ex, string std) nothrow @nogc @trusted
            {
                auto str = nogcFormat!"Now how cool is that!: %s"(ex);
                assert(str.startsWith("Now how cool is that!: bc.string.format.__unittest_L"));
                // import core.stdc.stdio; printf("%s\nvs\n%s\n", std.ptr, str.ptr);
                static if (__VERSION__ >= 2095)
                {
                    // we try to reflect last compiler behavior, previous might differ
                    assert(str[0..$] == std[0..$]);
                }
                else
                {
                    int ln = -1;
                    foreach (i, c; str[]) {
                        if (c=='\n') {
                            ln = cast(int)i;
                            break;
                        }
                    }

                    assert(ln>0);
                    assert(str[0..ln] == std[0..ln]);
                }
            }(ex, std);
        }
    }
}

/**
 * Gets size needed to hold formatted string result
 */
size_t getFormatSize(string fmt = "%s", ARGS...)(auto ref ARGS args) @safe nothrow @nogc
{
    NullSink ns;
    return ns.nogcFormatTo!fmt(args);
}

@("getFormatSize")
@safe unittest
{
    assert(getFormatSize!"foo" == 3);
    assert(getFormatSize!"foo=%d"(42) == 6);
    assert(getFormatSize!"%04d-%02d-%02dT%02d:%02d:%02d.%03d"(2020, 4, 28, 19, 20, 32, 207) == 23);
    assert(getFormatSize!"%x"(0x2C38) == 4);
    assert(getFormatSize!"%s"(9896) == 4);
}

/// pseudosink used just for calculation of resulting string length
struct NullSink {}

private enum FMT: ubyte {
    STR,
    CHR,
    DEC, // also for BOOL
    HEX,
    UHEX,
    PTR,
    FLT,
}

private struct FmtParams
{
    bool leftJustify; // Left justify the result in the field. It overrides any 0 flag.
    bool signed; // Prefix positive numbers in a signed conversion with a +. It overrides any space flag.
    bool prefixHex; // If non-zero, prefix result with 0x (0X).
    int width; // pad with characters, if -1, use previous argument as width
    char fill = ' '; // character to pad with
    int sep; // insert separator each X digits, if -1, use previous argument as X
    bool sepChar; // is separator char defined in additional arg?
    int prec; // precision, if -1, use previous argument as precision value
}

private bool isDigit()(immutable char c) { return c >= '0' && c <= '9'; }

// Parses format specifier in CTFE
// See: https://dlang.org/phobos/std_format.html for details
// Note: Just a subset of the specification is supported ATM. Parser here parses the spec, but
// formatter doesn't use it all.
//
// FormatStringItem:
//     '%%'
//     '%' Position Flags Width Separator Precision FormatChar
//     '%(' FormatString '%)'
//     '%-(' FormatString '%)'
//
auto formatSpec()(FMT f, string spec)
{
    FmtParams res; int idx;

    if (spec.length)
    {
        assert(spec.indexOf('$') < 0, "Position specifier not supported");

        // Flags:
        //   empty
        //   '-' Flags
        //   '+' Flags
        //   '#' Flags
        //   '0' Flags
        //   ' ' Flags
        while (idx < spec.length)
        {
            if (spec[idx] == '-') {
                res.leftJustify = true; idx++; continue;
            } else if (f.among(FMT.DEC, FMT.FLT) && spec[idx] == '+') {
                res.signed = true; idx++; continue;
            } else if (f == FMT.HEX && spec[idx] == '#') {
                // TODO: 'o' - Add to precision as necessary so that the first digit of the octal formatting is a '0', even if both the argument and the Precision are zero.
                res.prefixHex = true; idx++; continue;
            } else if (f == FMT.FLT && spec[idx] == '#') {
                // TODO: Always insert the decimal point and print trailing zeros.
                idx++; continue;
            } else if (f.among(FMT.DEC, FMT.FLT, FMT.HEX, FMT.UHEX, FMT.PTR) && spec[idx].among('0', ' ')) {
                res.fill = spec[idx++]; continue;
            }
            break;
        }

        if (idx == spec.length) goto done;

        // Width:
        //     empty
        //     Integer
        //     '*'
        if (spec[idx] == '*') { res.width = -1; idx++; }
        else {
            while (idx < spec.length && spec[idx].isDigit) res.width = res.width*10 + (spec[idx++] - '0');
        }

        if (idx == spec.length) goto done;

        // Separator:
        //     empty
        //     ','
        //     ',' '?'
        //     ',' '*' '?'
        //     ',' Integer '?'
        //     ',' '*'
        //     ',' Integer
        if (spec[idx] == ',') {
            // ie: writefln("'%,*?d'", 4, '$', 123456789);
            idx++;
            if (idx == spec.length) {res.sep = 3; goto done; }
            if (spec[idx].isDigit) {
                while (idx < spec.length && spec[idx].isDigit) res.sep = res.sep*10 + (spec[idx++] - '0');
            } else if (spec[idx] == '*') {
                idx++; res.sep = -1;
            } else res.sep = 3;

            if (idx == spec.length) goto done;
            if (spec[idx] == '?') { res.sepChar = true; idx++; }
        }
        if (idx == spec.length) goto done;

        // Precision:
        //     empty
        //     '.'
        //     '.' Integer
        //     '.*'
        if (spec[idx] == '.') {
            idx++;
            if (idx == spec.length) { res.prec = 6; goto done; }
            if (spec[idx].isDigit) {
                while (idx < spec.length && spec[idx].isDigit) res.prec = res.prec*10 + (spec[idx++] - '0');
            } else if (spec[idx] == '*') {
                idx++; res.prec = -1;
            }
        }
    }

done:
    assert(idx == spec.length, "Parser error");
    return res;
}

// Used to find end of the format specifier.
// See: https://dlang.org/phobos/std_format.html for grammar and valid characters for fmt spec
// Note: Nested array fmt spec is handled separately so no '(', ')' characters here
private ulong getNextNonDigitFrom()(string fmt)
{
    ulong idx;
    foreach (c; fmt) {
        if ("0123456789+-.,#*?$ ".indexOf(c) < 0)
            return idx;
        ++idx;
    }
    return idx;
}

private long getNestedArrayFmtLen()(string fmt)
{
    long idx; int lvl;
    while (idx < fmt.length)
    {
        // detect next level of nested array format spec
        if (fmt[idx] == '(' // new nested array can be '%(' or '%-('
            && (
                (idx > 0 && fmt[idx-1] == '%')
                || (idx > 1 && fmt[idx-2] == '%' && fmt[idx-1] == '-')
            )) lvl++;
        // detect end of nested array format spec
        if (fmt[idx] == '%' && fmt.length > idx+1 && fmt[idx+1] == ')') {
            if (!lvl) return idx+2;
            else --lvl;
        }
        ++idx;
    }
    return -1;
}

@("getNestedArrayFmtLen")
unittest
{
    static assert(getNestedArrayFmtLen("%d%)foo") == 4);
    static assert(getNestedArrayFmtLen("%d%| %)foo") == 7);
    static assert(getNestedArrayFmtLen("%(%d%)%)foo") == 8);
}

// workaround for std.string.indexOf not working in betterC
private ptrdiff_t indexOf()(string fmt, char c)
{
    for (ptrdiff_t i = 0; i < fmt.length; ++i)
        if (fmt[i] == c) return i;
    return -1;
}

// Phobos version has bug in CTFE, see: https://issues.dlang.org/show_bug.cgi?id=20783
private ptrdiff_t fixedLastIndexOf()(string s, string sub)
{
    if (!__ctfe) assert(0);

    LOOP: for (ptrdiff_t i = s.length - sub.length; i >= 0; --i)
    {
        version (D_BetterC)
        {
            // workaround for missing symbol used by DMD
            for (ptrdiff_t j=0; j<sub.length; ++j)
                if (s[i+j] != sub[j]) continue LOOP;
            return i;
        }
        else
        {
            if (s[i .. i + sub.length] == sub[])
                return i;
        }
    }
    return -1;
}

private template getNestedArrayFmt(string fmt)
{
    import std.meta : AliasSeq;

    // make sure we're searching in top level only
    enum lastSubEnd = fmt.fixedLastIndexOf("%)");
    static if (lastSubEnd > 0)
    {
        enum idx = fmt[lastSubEnd+2..$].fixedLastIndexOf("%|"); // delimiter separator used
        static if (idx >= 0)
            alias getNestedArrayFmt = AliasSeq!(fmt[0..lastSubEnd+2+idx], fmt[lastSubEnd+idx+4..$]);
        else
            alias getNestedArrayFmt = AliasSeq!(fmt[0..lastSubEnd+2], fmt[lastSubEnd+2..$]);
    }
    else
    {
        enum idx = fmt.fixedLastIndexOf("%|"); // delimiter separator used
        static if (idx >= 0)
            alias getNestedArrayFmt = AliasSeq!(fmt[0..idx], fmt[idx+2..$]); // we can return delimiter directly
        else {
            // we need to find end of inner fmt spec first
            static assert(fmt.length >= 2, "Invalid nested array element format specifier: " ~ fmt);
            enum startIdx = fmt.indexOf('%');
            static assert(startIdx >=0, "No nested array element format specified");
            enum endIdx = startIdx + 1 + getNextNonDigitFrom(fmt[startIdx + 1 .. $]);
            enum len = endIdx-startIdx+1;

            static if ((len == 2 && fmt[startIdx+1] == '(') || (len == 3 && fmt[startIdx+1..startIdx+3] == "-("))
            {
                // further nested array fmt spec -> split by end of nested highest level
                enum nlen = fmt[1] == '(' ? (2 + getNestedArrayFmtLen(fmt[2..$])) : (3 + getNestedArrayFmtLen(fmt[3..$]));
                static assert(nlen > 0, "Invalid nested array format specifier: " ~ fmt);
                alias getNestedArrayFmt = AliasSeq!(fmt[0..nlen], fmt[nlen..$]);
            }
            else
                // split at the end of element fmt spec
                alias getNestedArrayFmt = AliasSeq!(fmt[0..endIdx+1], fmt[endIdx+1..$]);
        }
    }
}

@("getNestedArrayFmt")
unittest
{
    import std.meta : AliasSeq;
    static assert(getNestedArrayFmt!"%d " == AliasSeq!("%d", " "));
    static assert(getNestedArrayFmt!"%d %|, " == AliasSeq!("%d ", ", "));
    static assert(getNestedArrayFmt!"%(%d %|, %)" == AliasSeq!("%(%d %|, %)", ""));
    static assert(getNestedArrayFmt!"%(%d %|, %),-" == AliasSeq!("%(%d %|, %)", ",-"));
    static assert(getNestedArrayFmt!"foo%(%d %|, %)-%|;" == AliasSeq!("foo%(%d %|, %)-", ";"));
}

private struct FmtSpec
{
    int idx;
    FMT type;
    string def;
}

// Nested array format specifier
private struct ArrFmtSpec
{
    int idx;
    string fmt; // item format
    string del; // delimiter
    bool esc; // escape strings and characters
}

/**
 * Splits format string based on the same rules as described here: https://dlang.org/phobos/std_format.html
 * In addition it supports 'p' as a pointer format specifier to be more compatible with `printf`.
 * It supports nested arrays format specifier too.
 */
template splitFmt(string fmt) {
    template spec(int j, FMT f, string def) {
        enum spec = FmtSpec(j, f, def);
    }
    template arrSpec(int j, string fmt, string del, bool esc) {
        enum arrSpec = ArrFmtSpec(j, fmt, del, esc);
    }

    template helper(int from, int j) {
        import std.typetuple : TypeTuple;
        enum idx = fmt[from .. $].indexOf('%');
        static if (idx < 0) {
            enum helper = TypeTuple!(fmt[from .. $]);
        }
        else {
            enum idx1 = idx + from;
            static if (idx1 >= fmt.length - 1) {
                static assert (false, "Expected formatter after %");
            } else {
                enum idx2 = idx1 + getNextNonDigitFrom(fmt[idx1+1 .. $]);
                // pragma(msg, "fmt: ", fmt[from .. idx2]);
                static if (fmt[idx2+1] == 's')
                    enum helper = TypeTuple!(fmt[from .. idx1], spec!(j, FMT.STR, fmt[idx1+1 .. idx2+1]), helper!(idx2+2, j+1));
                else static if (fmt[idx2+1] == 'c')
                    enum helper = TypeTuple!(fmt[from .. idx1], spec!(j, FMT.CHR, fmt[idx1+1 .. idx2+1]), helper!(idx2+2, j+1));
                else static if (fmt[idx2+1] == 'b') // TODO: should be binary, but use hex for now
                    enum helper = TypeTuple!(fmt[from .. idx1], spec!(j, FMT.HEX, fmt[idx1+1 .. idx2+1]), helper!(idx2+2, j+1));
                else static if (fmt[idx2+1].among('d', 'u'))
                    enum helper = TypeTuple!(fmt[from .. idx1], spec!(j, FMT.DEC, fmt[idx1+1 .. idx2+1]), helper!(idx2+2, j+1));
                else static if (fmt[idx2+1] == 'o') // TODO: should be octal, but use hex for now
                    enum helper = TypeTuple!(fmt[from .. idx1], spec!(j, FMT.DEC, fmt[idx1+1 .. idx2+1]), helper!(idx2+2, j+1));
                else static if (fmt[idx2+1] == 'x')
                    enum helper = TypeTuple!(fmt[from .. idx1], spec!(j, FMT.HEX, fmt[idx1+1 .. idx2+1]), helper!(idx2+2, j+1));
                else static if (fmt[idx2+1] == 'X')
                    enum helper = TypeTuple!(fmt[from .. idx1], spec!(j, FMT.UHEX, fmt[idx1+1 .. idx2+1]), helper!(idx2+2, j+1));
                else static if (fmt[idx2+1].among('e', 'E', 'f', 'F', 'g', 'G', 'a', 'A')) // TODO: float number formatters
                    enum helper = TypeTuple!(fmt[from .. idx1], spec!(j, FMT.FLT, fmt[idx1+1 .. idx2+1]), helper!(idx2+2, j+1));
                else static if (fmt[idx2+1] == 'p')
                    enum helper = TypeTuple!(fmt[from .. idx1], spec!(j, FMT.PTR, fmt[idx1+1 .. idx2+1]), helper!(idx2+2, j+1));
                else static if (fmt[idx2+1] == '%')
                    enum helper = TypeTuple!(fmt[from .. idx1+1], helper!(idx2+2, j));
                else static if (fmt[idx2+1] == '(' || fmt[idx2+1..idx2+3] == "-(") {
                    // nested array format specifier
                    enum l = fmt[idx2+1] == '('
                        ? getNestedArrayFmtLen(fmt[idx2+2..$])
                        : getNestedArrayFmtLen(fmt[idx2+3..$]);
                    alias naSpec = getNestedArrayFmt!(fmt[idx2+2 .. idx2+2+l-2]);
                    // pragma(msg, fmt[from .. idx1], "|", naSpec[0], "|", naSpec[1], "|");
                    enum helper = TypeTuple!(
                        fmt[from .. idx1],
                        arrSpec!(j, naSpec[0], naSpec[1], fmt[idx2+1] != '('),
                        helper!(idx2+2+l, j+1));
                }
                else static assert (false, "Invalid formatter '" ~ fmt[idx2+1] ~ "' in fmt='" ~ fmt ~ "'");
            }
        }
    }

    template countFormatters(tup...) {
        static if (tup.length == 0)
            enum countFormatters = 0;
        else static if (is(typeof(tup[0]) == FmtSpec) || is(typeof(tup[0]) == ArrFmtSpec))
            enum countFormatters = 1 + countFormatters!(tup[1 .. $]);
        else
            enum countFormatters = countFormatters!(tup[1 .. $]);
    }

    alias tokens = helper!(0, 0);
    alias numFormatters = countFormatters!tokens;
}

/// Returns string of enum member value
string enumToStr(E)(E value) pure @safe nothrow @nogc
{
    foreach (i, e; EnumMembers!E)
    {
        if (value == e) return __traits(allMembers, E)[i];
    }
    return null;
}

size_t formatPtr(S)(ref S sink, ulong p) pure @trusted nothrow @nogc
{
    pragma(inline);
    return formatPtr(sink, cast(void*)p);
}

size_t formatPtr(S)(ref S sink, const void* ptr) pure @safe nothrow @nogc
{
    pragma(inline);
    mixin SinkWriter!S;
    if (ptr is null) { write("null"); return 4; }
    else {
        import std.stdint : intptr_t;
        return sink.formatHex!((void*).sizeof*2)(cast(intptr_t)ptr);
    }
}

@("pointer")
@safe unittest
{
    char[100] buf;

    () @nogc @safe
    {
        assert(formatPtr(buf, 0x123) && buf[0..16] == "0000000000000123");
        assert(formatPtr(buf, 0) && buf[0..4] == "null");
        assert(formatPtr(buf, null) && buf[0..4] == "null");
    }();
}

alias Upper = Flag!"Upper";

pure @safe nothrow @nogc
size_t formatHex(size_t W = 0, char fill = '0', Upper upper = Upper.no, S)(ref S sink, ulong val)
{
    static if (is(S == NullSink))
    {
        // just formatted length calculation
        import std.algorithm : max;
        size_t len = 0;
        if (!val) len = 1;
        else { while (val) { val >>= 4; len++; } }
        return max(W, len);
    }
    else
    {
        mixin SinkWriter!S;

        size_t len = 0;
        auto v = val;
        char[16] buf = void;

        while (v) { v >>= 4; len++; }
        static if (W > 0) {
            if (W > len) {
                buf[0..W-len] = '0';
                len = W;
            }
        }

        v = val;
        if (v == 0) {
            static if (W == 0) { buf[0] = '0'; len = 1; } // special case for null
        }
        else {
            auto idx = len;
            while (v) {
                static if (upper)
                    buf[--idx] = "0123456789ABCDEF"[v & 0x0f];
                else buf[--idx] = "0123456789abcdef"[v & 0x0f];
                v >>= 4;
            }
        }

        write(buf[0..len]);
        return len;
    }
}

@("hexadecimal")
@safe @nogc unittest
{
    char[100] buf;
    assert(formatHex(buf, 0x123) && buf[0..3] == "123");
    assert(formatHex!10(buf, 0x123) && buf[0..10] == "0000000123");
    assert(formatHex(buf, 0) && buf[0..1] == "0");
    assert(formatHex!10(buf, 0) && buf[0..10] == "0000000000");
    assert(formatHex!10(buf, 0xa23456789) && buf[0..10] == "0a23456789");
    assert(formatHex!10(buf, 0x1234567890) && buf[0..10] == "1234567890");
    assert(formatHex!(10, '0', Upper.yes)(buf, 0x1234567890a) && buf[0..11] == "1234567890A");
}

size_t formatDecimal(size_t W = 0, char fillChar = ' ', S, T)(ref S sink, T val) pure @safe nothrow @nogc
    if (is(typeof({ulong v = val;})))
{
    import bc.string.numeric : numDigits;

    static if (is(Unqual!T == bool)) size_t len = 1;
    else size_t len = numDigits(val);

    static if (is(S == NullSink))
    {
        // just formatted length calculation
        import std.algorithm : max;
        return max(W, len);
    }
    else
    {
        mixin SinkWriter!S;

        ulong v;
        char[20] buf = void; // max number of digits for 8bit numbers is 20
        size_t idx;

        static if (isSigned!T)
        {
            import std.ascii : isWhite;
            if (_expect(val < 0, false))
            {
                if (_expect(val == long.min, false))
                {
                    // special case for unconvertable value
                    write("-9223372036854775808");
                    return 20;
                }

                static if (!isWhite(fillChar)) buf[idx++] = '-'; // to write minus character after padding
                v = -long(val);
            }
            else v = val;
        }
        else v = val;

        static if (W > 0) {
            if (W > len) {
                buf[idx .. idx + W - len] = fillChar;
                idx += W-len;
                len = W;
            }
        }

        static if (isSigned!T && isWhite(fillChar))
            if (val < 0) buf[idx++] = '-';

        if (v == 0) buf[idx++] = '0';
        else {
            idx = len;
            while (v) {
                buf[--idx] = "0123456789"[v % 10];
                v /= 10;
            }
        }

        write(buf[0..len]);
        return len;
    }
}

@("decimal")
@safe @nogc unittest
{
    char[100] buf;
    assert(formatDecimal!10(buf, -1234)  && buf[0..10] == "     -1234");
    assert(formatDecimal!10(buf, 0)      && buf[0..10] == "         0");
    assert(formatDecimal(buf, -1234)     && buf[0..5] == "-1234");
    assert(formatDecimal(buf, 0)         && buf[0..1] == "0");
    assert(formatDecimal!3(buf, 1234)    && buf[0..4] == "1234");
    assert(formatDecimal!3(buf, -1234)   && buf[0..5] == "-1234");
    assert(formatDecimal!3(buf, 0)       && buf[0..3] == "  0");
    assert(formatDecimal!(3,'0')(buf, 0) && buf[0..3] == "000");
    assert(formatDecimal!(3,'a')(buf, 0) && buf[0..3] == "aa0");
    assert(formatDecimal!(10, '0')(buf, -1234) && buf[0..10] == "-000001234");
    assert(formatDecimal(buf, true) && buf[0..1] == "1");
}

size_t formatFloat(S)(ref S sink, double val) @trusted nothrow @nogc // not pure with this implementation
{
    import core.stdc.stdio : snprintf;
    import std.algorithm : min;
    char[20] buf = void;
    auto len = snprintf(&buf[0], 20, "%g", val);
    len = min(len, 19);
    static if (!is(S == NullSink))
    {
        mixin SinkWriter!S;
        write(buf[0..len]);
    }
    return len;
}

@("float")
@safe unittest
{
    char[100] buf;
    assert(formatFloat(buf, 1.2345) && buf[0..6] == "1.2345");
    assert(formatFloat(buf, double.init) && buf[0..3] == "nan");
    assert(formatFloat(buf, double.infinity) && buf[0..3] == "inf");
}

size_t formatUUID(S)(ref S sink, UUID val) pure @safe nothrow @nogc
{
    static if (!is(S == NullSink))
    {
        import std.meta : AliasSeq;

        mixin SinkWriter!S;

        alias skipSeq = AliasSeq!(8, 13, 18, 23);
        alias byteSeq = AliasSeq!(0,2,4,6,9,11,14,16,19,21,24,26,28,30,32,34);

        char[36] buf = void;

        static foreach (pos; skipSeq)
            buf[pos] = '-';

        static foreach (i, pos; byteSeq)
        {
            buf[pos  ] = toChar!char(val.data[i] >> 4);
            buf[pos+1] = toChar!char(val.data[i] & 0x0F);
        }

        write(buf[0..36]);
    }
    return 36;
}

version (D_BetterC) {}
else
@("UUID")
@safe unittest
{
    char[100] buf;
    assert(formatUUID(buf, UUID([138, 179, 6, 14, 44, 186, 79, 35, 183, 76, 181, 45, 179, 189, 251, 70])) == 36);
    assert(buf[0..36] == "8ab3060e-2cba-4f23-b74c-b52db3bdfb46");
}

/**
 * Formats SysTime as ISO extended string.
 * Only UTC format supported.
 */
size_t formatSysTime(S)(ref S sink, SysTime val) @trusted nothrow @nogc // not pure because of gmtime_r
{
    mixin SinkWriter!S;

    // Note: we don't format based on the timezone set in SysTime, but just use UTC here
    enum hnsecsToUnixEpoch = 621_355_968_000_000_000L;
    enum hnsecsFrom1601    = 504_911_232_000_000_000L;

    static immutable char[7] invalidTimeBuf = "invalid";

    long time = __traits(getMember, val, "_stdTime"); // access private field
    long hnsecs = time % 10_000_000;

    // check for invalid time value
    version (Windows) {
        if (time < hnsecsFrom1601) { write(invalidTimeBuf); return invalidTimeBuf.length; }
    }

    static if (is(S == NullSink))
    {
        // just count required number of characters needed for hnsecs
        int len = 20; // fixed part for date time with just seconds resolution (including 'Z')
        if (hnsecs == 0) return len; // no fract seconds part
        len += 2; // dot and at least one number
        foreach (i; [1_000_000, 100_000, 10_000, 1_000, 100, 10])
        {
            hnsecs %= i;
            if (hnsecs == 0) break;
            len++;
        }
        return len;
    }
    else
    {
        char[28] buf; // maximal length for UTC extended ISO string

        version (Posix)
        {
            import core.sys.posix.sys.types : time_t;
            import core.sys.posix.time : gmtime_r, tm;

            time -= hnsecsToUnixEpoch; // convert to unix time but still with hnsecs

            // split to hnsecs and time in seconds
            time_t unixTime = time / 10_000_000;

            tm timeSplit;
            gmtime_r(&unixTime, &timeSplit);

            buf.nogcFormatTo!"%04d-%02d-%02dT%02d:%02d:%02d"(
                timeSplit.tm_year + 1900,
                timeSplit.tm_mon + 1,
                timeSplit.tm_mday,
                timeSplit.tm_hour,
                timeSplit.tm_min,
                timeSplit.tm_sec
            );
        }
        else version (Windows)
        {
            import core.sys.windows.winbase : FILETIME, FileTimeToSystemTime, SYSTEMTIME;
            import core.sys.windows.winnt : ULARGE_INTEGER;

            ULARGE_INTEGER ul;
            ul.QuadPart = cast(ulong)time - hnsecsFrom1601;

            FILETIME ft;
            ft.dwHighDateTime = ul.HighPart;
            ft.dwLowDateTime = ul.LowPart;

            SYSTEMTIME stime;
            FileTimeToSystemTime(&ft, &stime);

            buf.nogcFormatTo!"%04d-%02d-%02dT%02d:%02d:%02d"(
                stime.wYear,
                stime.wMonth,
                stime.wDay,
                stime.wHour,
                stime.wMinute,
                stime.wSecond
            );
        }
        else static assert(0, "SysTime format not supported for this platform yet");

        if (hnsecs == 0)
        {
            buf[19] = 'Z';
            write(buf[0..20]);
            return 20;
        }

        buf[19] = '.';

        int len = 20;
        foreach (i; [1_000_000, 100_000, 10_000, 1_000, 100, 10, 1])
        {
            buf[len++] = cast(char)(hnsecs / i + '0');
            hnsecs %= i;
            if (hnsecs == 0) break;
        }
        buf[len++] = 'Z';
        write(buf[0..len]);
        return len;
    }
}

version (D_BetterC) {}
else
@("SysTime")
@safe unittest
{
    char[100] buf;

    assert(formatSysTime(buf, SysTime.fromISOExtString("2020-06-08T14:25:30.1234567Z")) == 28);
    assert(buf[0..28] == "2020-06-08T14:25:30.1234567Z");
    assert(formatSysTime(buf, SysTime.fromISOExtString("2020-06-08T14:25:30.123456Z")) == 27);
    assert(buf[0..27] == "2020-06-08T14:25:30.123456Z");
    assert(formatSysTime(buf, SysTime.fromISOExtString("2020-06-08T14:25:30.12345Z")) == 26);
    assert(buf[0..26] == "2020-06-08T14:25:30.12345Z");
    assert(formatSysTime(buf, SysTime.fromISOExtString("2020-06-08T14:25:30.1234Z")) == 25);
    assert(buf[0..25] == "2020-06-08T14:25:30.1234Z");
    assert(formatSysTime(buf, SysTime.fromISOExtString("2020-06-08T14:25:30.123Z")) == 24);
    assert(buf[0..24] == "2020-06-08T14:25:30.123Z");
    assert(formatSysTime(buf, SysTime.fromISOExtString("2020-06-08T14:25:30.12Z")) == 23);
    assert(buf[0..23] == "2020-06-08T14:25:30.12Z");
    assert(formatSysTime(buf, SysTime.fromISOExtString("2020-06-08T14:25:30.1Z")) == 22);
    assert(buf[0..22] == "2020-06-08T14:25:30.1Z");
    assert(formatSysTime(buf, SysTime.fromISOExtString("2020-06-08T14:25:30Z")) == 20);
    assert(buf[0..20] == "2020-06-08T14:25:30Z");
    version (Posix) {
        assert(formatSysTime(buf, SysTime.init) == 20);
        assert(buf[0..20] == "0001-01-01T00:00:00Z");
    }
    else version (Windows) {
        assert(formatSysTime(buf, SysTime.init) == 7);
        assert(buf[0..7] == "invalid");
    }

    assert(getFormatSize!"%s"(SysTime.fromISOExtString("2020-06-08T14:25:30.1234567Z")) == 28);
    assert(getFormatSize!"%s"(SysTime.fromISOExtString("2020-06-08T14:25:30.123456Z")) == 27);
    assert(getFormatSize!"%s"(SysTime.fromISOExtString("2020-06-08T14:25:30.12345Z")) == 26);
    assert(getFormatSize!"%s"(SysTime.fromISOExtString("2020-06-08T14:25:30.1234Z")) == 25);
    assert(getFormatSize!"%s"(SysTime.fromISOExtString("2020-06-08T14:25:30.123Z")) == 24);
    assert(getFormatSize!"%s"(SysTime.fromISOExtString("2020-06-08T14:25:30.12Z")) == 23);
    assert(getFormatSize!"%s"(SysTime.fromISOExtString("2020-06-08T14:25:30.1Z")) == 22);
    assert(getFormatSize!"%s"(SysTime.fromISOExtString("2020-06-08T14:25:30Z")) == 20);
}

/**
 * Formats duration.
 * It uses custom formatter that is inspired by std.format output, but a bit shorter.
 * Note: ISO 8601 was considered, but it's not as human readable as used format.
 */
size_t formatDuration(S)(ref S sink, Duration val) @trusted nothrow @nogc pure
{
    mixin SinkWriter!S;

    enum secondsInDay = 86_400;
    enum secondsInHour = 3_600;
    enum secondsInMinute = 60;

    long totalHNS = __traits(getMember, val, "_hnsecs"); // access private member
    if (totalHNS < 0) { write("-"); totalHNS = -totalHNS; }

    immutable long fracSecs = totalHNS % 10_000_000;
    long totalSeconds = totalHNS / 10_000_000;

    if (totalSeconds)
    {
        immutable long days = totalSeconds / secondsInDay;
        long seconds = totalSeconds % secondsInDay;
        if (days) advance(s.nogcFormatTo!"%d days"(days));
        if (seconds)
        {
            immutable hours = seconds / secondsInHour;
            seconds %= secondsInHour;
            if (hours)
                advance(days ? s.nogcFormatTo!", %d hrs"(hours) : s.nogcFormatTo!"%d hrs"(hours));

            if (seconds)
            {
                immutable minutes = seconds / secondsInMinute;
                seconds %= secondsInMinute;
                if (minutes)
                    advance(days || hours ? s.nogcFormatTo!", %d mins"(minutes) : s.nogcFormatTo!"%d mins"(minutes));

                if (seconds)
                    advance(days || hours || minutes ? s.nogcFormatTo!", %d secs"(seconds) : s.nogcFormatTo!"%d secs"(seconds));
            }
        }
    }

    if (fracSecs)
    {
        immutable msecs = fracSecs / 10_000;
        int usecs = fracSecs % 10_000;

        if (msecs | usecs)
        {
            advance(totalSeconds ? s.nogcFormatTo!", %d"(msecs) : s.nogcFormatTo!"%d"(msecs));

            if (usecs)
            {
                char[5] buf = void;
                buf[0] = '.';

                int ulen = 1;
                foreach (i; [1_000, 100, 10, 1])
                {
                    buf[ulen++] = cast(char)(usecs / i + '0');
                    usecs %= i;
                    if (usecs == 0) break;
                }
                write(buf[0..ulen]);
            }

            write(" ms");
        }
    }

    if (!totalLen) write("0 ms");

    return totalLen;
}

version (D_BetterC) {}
else
@("duration")
@safe unittest
{
    import core.time;
    char[100] buf;

    assert(formatDuration(buf, 1.seconds) == 6);
    assert(buf[0..6] == "1 secs");

    assert(formatDuration(buf, 1.seconds + 15.msecs + 5.hnsecs) == 18);
    assert(buf[0..18] == "1 secs, 15.0005 ms");

    assert(formatDuration(buf, 1.seconds + 1215.msecs + 15.hnsecs) == 19);
    assert(buf[0..19] == "2 secs, 215.0015 ms");

    assert(formatDuration(buf, 5.days) == 6);
    assert(buf[0..6] == "5 days");

    assert(formatDuration(buf, 5.days + 25.hours) == 13);
    assert(buf[0..13] == "6 days, 1 hrs");

    assert(formatDuration(buf, 5.days + 25.hours + 78.minutes) == 22);
    assert(buf[0..22] == "6 days, 2 hrs, 18 mins");

    assert(formatDuration(buf, 5.days + 25.hours + 78.minutes + 102.seconds) == 31);
    assert(buf[0..31] == "6 days, 2 hrs, 19 mins, 42 secs");

    assert(formatDuration(buf, 5.days + 25.hours + 78.minutes + 102.seconds + 2321.msecs) == 39);
    assert(buf[0..39] == "6 days, 2 hrs, 19 mins, 44 secs, 321 ms");

    assert(formatDuration(buf, 5.days + 25.hours + 78.minutes + 102.seconds + 2321.msecs + 1987.usecs) == 43);
    assert(buf[0..43] == "6 days, 2 hrs, 19 mins, 44 secs, 322.987 ms");

    assert(formatDuration(buf, 5.days + 25.hours + 78.minutes + 102.seconds + 2321.msecs + 1987.usecs + 15.hnsecs) == 44);
    assert(buf[0..44] == "6 days, 2 hrs, 19 mins, 44 secs, 322.9885 ms");

    assert(formatDuration(buf, -42.msecs) == 6);
    assert(buf[0..6] == "-42 ms");

    assert(formatDuration(buf, Duration.zero) == 4);
    assert(buf[0..4] == "0 ms");
}

@safe pure nothrow @nogc Char toChar(Char)(size_t i)
{
    pragma(inline);
    if (i <= 9) return cast(Char)('0' + i);
    else return cast(Char)('a' + (i-10));
}

/// Output range wrapper for used sinks (so it can be used in toString functions)
private struct SinkWrap(S)
{
    private S s;

    static if (isArray!S && is(ForeachType!S : char))
        mixin SinkWriter!(S, false);
    else static if (isPointer!S)
        mixin SinkWriter!(PointerTarget!S, false);
    else static assert(0, "Unsupported sink type: " ~ S.stringof);

    this(S sink) @safe pure nothrow @nogc
    {
        this.s = sink;
    }
}

// helper to create `SinkWrap` that handles various sink types
private auto sinkWrap(S)(ref S sink) @trusted // we're only using this internally and don't escape the pointer
{
    static if (isStaticArray!S && is(ForeachType!S : char))
        return SinkWrap!(char[])(sink[]); // we need to slice it
    else static if (is(S == struct))
        return SinkWrap!(S*)(&sink); // work with a pointer to an original sink (ie `MallocBuffer`)
    else static assert(0, "Unsupported sink type: " ~ S.stringof);
}

@("sink wrapper")
unittest
{
    char[42] buf;
    auto sink = sinkWrap(buf);
    sink.put("foo");
    assert(sink.totalLen == 3);
    assert(buf[0..3] == "foo");
}

// helper functions used in formatters to write formatted string to sink
private mixin template SinkWriter(S, bool field = true)
{
    size_t totalLen;
    static if (isArray!S && is(ForeachType!S : char))
    {
        static if (field) char[] s = sink[];

        @nogc pure nothrow @trusted
        {
            void advance(size_t len)
            {
                s = s[len..$];
                totalLen += len;
            }

            void write(const(char)[] str) {
                s[0..str.length] = str;
                advance(str.length);
            }

            void write(char ch) {
                s[0] = ch;
                advance(1);
            }
        }
    }
    else static if (is(S == NullSink))
    {
        static if (field) alias s = sink;
        void advance(size_t len) { totalLen += len; }
        void write(const(char)[] str) { advance(str.length); }
        void write(char ch) { advance(1); }
    }
    else
    {
        static if (field) alias s = sink;
        @nogc pure nothrow @safe
        {
            import std.range : rput = put;
            void advance(size_t len) { totalLen += len; }
            void write(const(char)[] str) { rput(s, str); advance(str.length); }
            void write(char ch) { rput(s, ch); advance(1); }
        }
    }

    alias put = write; // output range interface
}
