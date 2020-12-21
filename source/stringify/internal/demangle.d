/**
 * Copy from druntime available module buth with modifications allowing it's usage in @nogc.
 *
 * Warning: This is currently used in backtrace printing in which case buffer to demangle into is a
 * slice to static array (1024B). be aware that this @nogc variant is altered in a way, that it just
 * asserts in case of more data is needed. Backtrace printing is handled this way in druntime (as
 * it'll be a problem when demangle'll resize static array slice), so it should be ok for us too.
 * Just be carefull about that.
 *
 * All modifications to the original are noted in comments with 'CHANGE:'.
 *
 * See: https://github.com/dlang/druntime/blob/master/src/core/demangle.d
 * Last revision from: d2d49ab4930e1ffd3ece6e5a6ea7767a01d5e077
 */
module stringify.internal.demangle;

import core.exception : onOutOfMemoryErrorNoGC;

struct NoHooks {}

// NOTE: Copy from https://github.com/dlang/druntime/blob/master/src/core/demangle.d (commit 99534d2595aa8a4851430de3c276308b829236b7)
struct Demangle(Hooks = NoHooks)
{
    // NOTE: This implementation currently only works with mangled function
    //       names as they exist in an object file.  Type names mangled via
    //       the .mangleof property are effectively incomplete as far as the
    //       ABI is concerned and so are not considered to be mangled symbol
    //       names.

    // NOTE: This implementation builds the demangled buffer in place by
    //       writing data as it is decoded and then rearranging it later as
    //       needed.  In practice this results in very little data movement,
    //       and the performance cost is more than offset by the gain from
    //       not allocating dynamic memory to assemble the name piecemeal.
    //
    //       If the destination buffer is too small, parsing will restart
    //       with a larger buffer.  Since this generally means only one
    //       allocation during the course of a parsing run, this is still
    //       faster than assembling the result piecemeal.

pure @safe @nogc: // CHANGE: added @nogc
    enum AddType { no, yes }


    this( return const(char)[] buf_, return char[] dst_ = null )
    {
        this( buf_, AddType.yes, dst_ );
    }


    this( return const(char)[] buf_, AddType addType_, return char[] dst_ = null )
    {
        buf     = buf_;
        addType = addType_;
        dst     = dst_;
    }


    enum size_t minBufSize = 4000;


    const(char)[]   buf     = null;
    char[]          dst     = null;
    size_t          pos     = 0;
    size_t          len     = 0;
    size_t          brp     = 0; // current back reference pos
    AddType         addType = AddType.yes;
    bool            mute    = false;
    Hooks           hooks;

    static class ParseException : Exception
    {
        @safe pure nothrow this( string msg )
        {
            super( msg );
        }
    }


    static class OverflowException : Exception
    {
        @safe pure nothrow this( string msg )
        {
            super( msg );
        }
    }


    static void error( string msg = "Invalid symbol" ) @trusted /* exception only used in module */
    {
        pragma(inline, false); // tame dmd inliner

        //throw new ParseException( msg );
        debug(info) printf( "error: %.*s\n", cast(int) msg.length, msg.ptr );
        throw cast(ParseException) cast(void*) typeid(ParseException).initializer; // CHANGE: not used in ctfe
    }


    static void overflow( string msg = "Buffer overflow" ) @trusted /* exception only used in module */
    {
        pragma(inline, false); // tame dmd inliner

        //throw new OverflowException( msg );
        debug(info) printf( "overflow: %.*s\n", cast(int) msg.length, msg.ptr );
        throw cast(OverflowException) cast(void*) typeid(OverflowException).initializer;
    }


    //////////////////////////////////////////////////////////////////////////
    // Type Testing and Conversion
    //////////////////////////////////////////////////////////////////////////


    static bool isAlpha( char val )
    {
        return ('a' <= val && 'z' >= val) ||
               ('A' <= val && 'Z' >= val) ||
               (0x80 & val); // treat all unicode as alphabetic
    }


    static bool isDigit( char val )
    {
        return '0' <= val && '9' >= val;
    }


    static bool isHexDigit( char val )
    {
        return ('0' <= val && '9' >= val) ||
               ('a' <= val && 'f' >= val) ||
               ('A' <= val && 'F' >= val);
    }


    static ubyte ascii2hex( char val )
    {
        if (val >= 'a' && val <= 'f')
            return cast(ubyte)(val - 'a' + 10);
        if (val >= 'A' && val <= 'F')
            return cast(ubyte)(val - 'A' + 10);
        if (val >= '0' && val <= '9')
            return cast(ubyte)(val - '0');
        error();
        return 0;
    }


    //////////////////////////////////////////////////////////////////////////
    // Data Output
    //////////////////////////////////////////////////////////////////////////


    static bool contains( const(char)[] a, const(char)[] b ) @trusted
    {
        if (a.length && b.length)
        {
            auto bend = b.ptr + b.length;
            auto aend = a.ptr + a.length;
            return a.ptr <= b.ptr && bend <= aend;
        }
        return false;
    }


    // move val to the end of the dst buffer
    char[] shift( const(char)[] val )
    {
        pragma(inline, false); // tame dmd inliner

        if ( val.length && !mute )
        {
            assert( contains( dst[0 .. len], val ) );
            debug(info) printf( "shifting (%.*s)\n", cast(int) val.length, val.ptr );

            if (len + val.length > dst.length)
                overflow();
            size_t v = &val[0] - &dst[0];
            dst[len .. len + val.length] = val[];
            for (size_t p = v; p < len; p++)
                dst[p] = dst[p + val.length];

            return dst[len - val.length .. len];
        }
        return null;
    }

    // remove val from dst buffer
    void remove( const(char)[] val )
    {
        pragma(inline, false); // tame dmd inliner

        if ( val.length )
        {
            assert( contains( dst[0 .. len], val ) );
            debug(info) printf( "removing (%.*s)\n", cast(int) val.length, val.ptr );
            size_t v = &val[0] - &dst[0];
            assert( len >= val.length && len <= dst.length );
            len -= val.length;
            for (size_t p = v; p < len; p++)
                dst[p] = dst[p + val.length];
        }
    }

    char[] append( const(char)[] val ) return scope
    {
        pragma(inline, false); // tame dmd inliner

        if ( val.length && !mute )
        {
            if ( !dst.length )
                // dst.length = minBufSize;
                assert(0, "out of range"); // CHANGE: can't realloc slice of the provided static array anyway
            assert( !contains( dst[0 .. len], val ) );
            debug(info) printf( "appending (%.*s)\n", cast(int) val.length, val.ptr );

            if ( dst.length - len >= val.length && &dst[len] == &val[0] )
            {
                // data is already in place
                auto t = dst[len .. len + val.length];
                len += val.length;
                return t;
            }
            if ( dst.length - len >= val.length )
            {
                dst[len .. len + val.length] = val[];
                auto t = dst[len .. len + val.length];
                len += val.length;
                return t;
            }
            overflow();
        }
        return null;
    }

    void putComma(size_t n)
    {
        pragma(inline, false);
        if (n)
            put(", ");
    }

    char[] put(char c) return scope
    {
        char[1] val = c;
        return put(val[]);
    }

    char[] put( scope const(char)[] val ) return scope
    {
        pragma(inline, false); // tame dmd inliner

        if ( val.length )
        {
            if ( !contains( dst[0 .. len], val ) )
                return append( val );
            return shift( val );
        }
        return null;
    }


    void putAsHex( size_t val, int width = 0 )
    {
        import core.internal.string;

        UnsignedStringBuf buf = void;

        static if (__VERSION__ >= 2094) auto s = unsignedToTempString!16(val, buf);
        else auto s = unsignedToTempString(val, buf, 16);
        int slen = cast(int)s.length;
        if (slen < width)
        {
            foreach (i; slen .. width)
                put('0');
        }
        put(s);
    }


    void pad( const(char)[] val )
    {
        if ( val.length )
        {
            append( " " );
            put( val );
        }
    }


    // CHANGE: reworked
    // void silent( void delegate() pure @safe @nogc dg )
    // {
    //     debug(trace) printf( "silent+\n" );
    //     debug(trace) scope(success) printf( "silent-\n" );
    //     auto n = len; dg(); len = n;
    // }


    //////////////////////////////////////////////////////////////////////////
    // Parsing Utility
    //////////////////////////////////////////////////////////////////////////

    @property bool empty()
    {
        return pos >= buf.length;
    }

    @property char front()
    {
        if ( pos < buf.length )
            return buf[pos];
        return char.init;
    }

    char peek( size_t n )
    {
        if ( pos + n < buf.length )
            return buf[pos + n];
        return char.init;
    }


    void test( char val )
    {
        if ( val != front )
            error();
    }


    void popFront()
    {
        if ( pos++ >= buf.length )
            error();
    }


    void match( char val )
    {
        test( val );
        popFront();
    }


    void match( const(char)[] val )
    {
        foreach (char e; val )
        {
            test( e );
            popFront();
        }
    }


    void eat( char val )
    {
        if ( val == front )
            popFront();
    }

    bool isSymbolNameFront()
    {
        char val = front;
        if ( isDigit( val ) || val == '_' )
            return true;
        if ( val != 'Q' )
            return false;

        // check the back reference encoding after 'Q'
        val = peekBackref();
        return isDigit( val ); // identifier ref
    }

    // return the first character at the back reference
    char peekBackref()
    {
        assert( front == 'Q' );
        auto n = decodeBackref!1();
        if (!n || n > pos)
            error("invalid back reference");

        return buf[pos - n];
    }

    size_t decodeBackref(size_t peekAt = 0)()
    {
        enum base = 26;
        size_t n = 0;
        for (size_t p; ; p++)
        {
            char t;
            static if (peekAt > 0)
            {
                t = peek(peekAt + p);
            }
            else
            {
                t = front;
                popFront();
            }
            if (t < 'A' || t > 'Z')
            {
                if (t < 'a' || t > 'z')
                    error("invalid back reference");
                n = base * n + t - 'a';
                return n;
            }
            n = base * n + t - 'A';
        }
    }

    //////////////////////////////////////////////////////////////////////////
    // Parsing Implementation
    //////////////////////////////////////////////////////////////////////////


    /*
    Number:
        Digit
        Digit Number
    */
    const(char)[] sliceNumber() return scope
    {
        debug(trace) printf( "sliceNumber+\n" );
        debug(trace) scope(success) printf( "sliceNumber-\n" );

        auto beg = pos;

        while ( true )
        {
            auto t = front;
            if (t >= '0' && t <= '9')
                popFront();
            else
                return buf[beg .. pos];
        }
    }


    size_t decodeNumber() scope
    {
        debug(trace) printf( "decodeNumber+\n" );
        debug(trace) scope(success) printf( "decodeNumber-\n" );

        return decodeNumber( sliceNumber() );
    }


    size_t decodeNumber( scope const(char)[] num ) scope
    {
        debug(trace) printf( "decodeNumber+\n" );
        debug(trace) scope(success) printf( "decodeNumber-\n" );

        size_t val = 0;

        foreach ( c; num )
        {
            import core.checkedint : mulu, addu;

            bool overflow = false;
            val = mulu(val, 10, overflow);
            val = addu(val, c - '0',  overflow);
            if (overflow)
                error();
        }
        return val;
    }


    void parseReal() scope
    {
        debug(trace) printf( "parseReal+\n" );
        debug(trace) scope(success) printf( "parseReal-\n" );

        char[64] tbuf = void;
        size_t   tlen = 0;
        real     val  = void;

        if ( 'I' == front )
        {
            match( "INF" );
            put( "real.infinity" );
            return;
        }
        if ( 'N' == front )
        {
            popFront();
            if ( 'I' == front )
            {
                match( "INF" );
                put( "-real.infinity" );
                return;
            }
            if ( 'A' == front )
            {
                match( "AN" );
                put( "real.nan" );
                return;
            }
            tbuf[tlen++] = '-';
        }

        tbuf[tlen++] = '0';
        tbuf[tlen++] = 'X';
        if ( !isHexDigit( front ) )
            error( "Expected hex digit" );
        tbuf[tlen++] = front;
        tbuf[tlen++] = '.';
        popFront();

        while ( isHexDigit( front ) )
        {
            tbuf[tlen++] = front;
            popFront();
        }
        match( 'P' );
        tbuf[tlen++] = 'p';
        if ( 'N' == front )
        {
            tbuf[tlen++] = '-';
            popFront();
        }
        else
        {
            tbuf[tlen++] = '+';
        }
        while ( isDigit( front ) )
        {
            tbuf[tlen++] = front;
            popFront();
        }

        tbuf[tlen] = 0;
        debug(info) printf( "got (%s)\n", tbuf.ptr );
        pureReprintReal( tbuf[] );
        debug(info) printf( "converted (%.*s)\n", cast(int) tlen, tbuf.ptr );
        put( tbuf[0 .. tlen] );
    }


    /*
    LName:
        Number Name

    Name:
        Namestart
        Namestart Namechars

    Namestart:
        _
        Alpha

    Namechar:
        Namestart
        Digit

    Namechars:
        Namechar
        Namechar Namechars
    */
    void parseLName() scope
    {
        debug(trace) printf( "parseLName+\n" );
        debug(trace) scope(success) printf( "parseLName-\n" );

        static if (__traits(hasMember, Hooks, "parseLName"))
            if (hooks.parseLName(this))
                return;

        if ( front == 'Q' )
        {
            // back reference to LName
            auto refPos = pos;
            popFront();
            size_t n = decodeBackref();
            if ( !n || n > refPos )
                error( "Invalid LName back reference" );
            if ( !mute )
            {
                auto savePos = pos;
                scope(exit) pos = savePos;
                pos = refPos - n;
                parseLName();
            }
            return;
        }
        auto n = decodeNumber();
        if ( n == 0 )
        {
            put( "__anonymous" );
            return;
        }
        if ( n > buf.length || n > buf.length - pos )
            error( "LName must be at least 1 character" );
        if ( '_' != front && !isAlpha( front ) )
            error( "Invalid character in LName" );
        foreach (char e; buf[pos + 1 .. pos + n] )
        {
            if ( '_' != e && !isAlpha( e ) && !isDigit( e ) )
                error( "Invalid character in LName" );
        }

        put( buf[pos .. pos + n] );
        pos += n;
    }


    /*
    Type:
        Shared
        Const
        Immutable
        Wild
        TypeArray
        TypeVector
        TypeStaticArray
        TypeAssocArray
        TypePointer
        TypeFunction
        TypeIdent
        TypeClass
        TypeStruct
        TypeEnum
        TypeTypedef
        TypeDelegate
        TypeNone
        TypeVoid
        TypeByte
        TypeUbyte
        TypeShort
        TypeUshort
        TypeInt
        TypeUint
        TypeLong
        TypeUlong
        TypeCent
        TypeUcent
        TypeFloat
        TypeDouble
        TypeReal
        TypeIfloat
        TypeIdouble
        TypeIreal
        TypeCfloat
        TypeCdouble
        TypeCreal
        TypeBool
        TypeChar
        TypeWchar
        TypeDchar
        TypeTuple

    Shared:
        O Type

    Const:
        x Type

    Immutable:
        y Type

    Wild:
        Ng Type

    TypeArray:
        A Type

    TypeVector:
        Nh Type

    TypeStaticArray:
        G Number Type

    TypeAssocArray:
        H Type Type

    TypePointer:
        P Type

    TypeFunction:
        CallConvention FuncAttrs Arguments ArgClose Type

    TypeIdent:
        I LName

    TypeClass:
        C LName

    TypeStruct:
        S LName

    TypeEnum:
        E LName

    TypeTypedef:
        T LName

    TypeDelegate:
        D TypeFunction

    TypeNone:
        n

    TypeVoid:
        v

    TypeByte:
        g

    TypeUbyte:
        h

    TypeShort:
        s

    TypeUshort:
        t

    TypeInt:
        i

    TypeUint:
        k

    TypeLong:
        l

    TypeUlong:
        m

    TypeCent
        zi

    TypeUcent
        zk

    TypeFloat:
        f

    TypeDouble:
        d

    TypeReal:
        e

    TypeIfloat:
        o

    TypeIdouble:
        p

    TypeIreal:
        j

    TypeCfloat:
        q

    TypeCdouble:
        r

    TypeCreal:
        c

    TypeBool:
        b

    TypeChar:
        a

    TypeWchar:
        u

    TypeDchar:
        w

    TypeTuple:
        B Number Arguments
    */
    char[] parseType( char[] name = null ) return scope
    {
        static immutable string[23] primitives = [
            "char", // a
            "bool", // b
            "creal", // c
            "double", // d
            "real", // e
            "float", // f
            "byte", // g
            "ubyte", // h
            "int", // i
            "ireal", // j
            "uint", // k
            "long", // l
            "ulong", // m
            null, // n
            "ifloat", // o
            "idouble", // p
            "cfloat", // q
            "cdouble", // r
            "short", // s
            "ushort", // t
            "wchar", // u
            "void", // v
            "dchar", // w
        ];

        static if (__traits(hasMember, Hooks, "parseType"))
            if (auto n = hooks.parseType(this, name))
                return n;

        debug(trace) printf( "parseType+\n" );
        debug(trace) scope(success) printf( "parseType-\n" );
        auto beg = len;
        auto t = front;

        char[] parseBackrefType(scope char[] delegate() pure @safe @nogc parseDg) pure @safe
        {
            if (pos == brp)
                error("recursive back reference");
            auto refPos = pos;
            popFront();
            auto n = decodeBackref();
            if (n == 0 || n > pos)
                error("invalid back reference");
            if ( mute )
                return null;
            auto savePos = pos;
            auto saveBrp = brp;
            scope(success) { pos = savePos; brp = saveBrp; }
            pos = refPos - n;
            brp = refPos;
            auto ret = parseDg();
            return ret;
        }

        switch ( t )
        {
        case 'Q': // Type back reference
            return parseBackrefType( () @nogc => parseType( name ) );
        case 'O': // Shared (O Type)
            popFront();
            put( "shared(" );
            parseType();
            put( ')' );
            pad( name );
            return dst[beg .. len];
        case 'x': // Const (x Type)
            popFront();
            put( "const(" );
            parseType();
            put( ')' );
            pad( name );
            return dst[beg .. len];
        case 'y': // Immutable (y Type)
            popFront();
            put( "immutable(" );
            parseType();
            put( ')' );
            pad( name );
            return dst[beg .. len];
        case 'N':
            popFront();
            switch ( front )
            {
            case 'g': // Wild (Ng Type)
                popFront();
                // TODO: Anything needed here?
                put( "inout(" );
                parseType();
                put( ')' );
                return dst[beg .. len];
            case 'h': // TypeVector (Nh Type)
                popFront();
                put( "__vector(" );
                parseType();
                put( ')' );
                return dst[beg .. len];
            default:
                error();
                assert( 0 );
            }
        case 'A': // TypeArray (A Type)
            popFront();
            parseType();
            put( "[]" );
            pad( name );
            return dst[beg .. len];
        case 'G': // TypeStaticArray (G Number Type)
            popFront();
            auto num = sliceNumber();
            parseType();
            put( '[' );
            put( num );
            put( ']' );
            pad( name );
            return dst[beg .. len];
        case 'H': // TypeAssocArray (H Type Type)
            popFront();
            // skip t1
            auto tx = parseType();
            parseType();
            put( '[' );
            put( tx );
            put( ']' );
            pad( name );
            return dst[beg .. len];
        case 'P': // TypePointer (P Type)
            popFront();
            parseType();
            put( '*' );
            pad( name );
            return dst[beg .. len];
        case 'F': case 'U': case 'W': case 'V': case 'R': // TypeFunction
            return parseTypeFunction( name );
        case 'C': // TypeClass (C LName)
        case 'S': // TypeStruct (S LName)
        case 'E': // TypeEnum (E LName)
        case 'T': // TypeTypedef (T LName)
            popFront();
            parseQualifiedName();
            pad( name );
            return dst[beg .. len];
        case 'D': // TypeDelegate (D TypeFunction)
            popFront();
            auto modbeg = len;
            parseModifier();
            auto modend = len;
            if ( front == 'Q' )
                parseBackrefType( () => parseTypeFunction( name, IsDelegate.yes ) );
            else
                parseTypeFunction( name, IsDelegate.yes );
            if (modend > modbeg)
            {
                // move modifiers behind the function arguments
                shift(dst[modend-1 .. modend]); // trailing space
                shift(dst[modbeg .. modend-1]);
            }
            return dst[beg .. len];
        case 'n': // TypeNone (n)
            popFront();
            // TODO: Anything needed here?
            return dst[beg .. len];
        case 'B': // TypeTuple (B Number Arguments)
            popFront();
            // TODO: Handle this.
            return dst[beg .. len];
        case 'Z': // Internal symbol
            // This 'type' is used for untyped internal symbols, i.e.:
            // __array
            // __init
            // __vtbl
            // __Class
            // __Interface
            // __ModuleInfo
            popFront();
            return dst[beg .. len];
        default:
            if (t >= 'a' && t <= 'w')
            {
                popFront();
                put( primitives[cast(size_t)(t - 'a')] );
                pad( name );
                return dst[beg .. len];
            }
            else if (t == 'z')
            {
                popFront();
                switch ( front )
                {
                case 'i':
                    popFront();
                    put( "cent" );
                    pad( name );
                    return dst[beg .. len];
                case 'k':
                    popFront();
                    put( "ucent" );
                    pad( name );
                    return dst[beg .. len];
                default:
                    error();
                    assert( 0 );
                }
            }
            error();
            return null;
        }
    }


    /*
    TypeFunction:
        CallConvention FuncAttrs Arguments ArgClose Type

    CallConvention:
        F       // D
        U       // C
        W       // Windows
        V       // Pascal
        R       // C++

    FuncAttrs:
        FuncAttr
        FuncAttr FuncAttrs

    FuncAttr:
        empty
        FuncAttrPure
        FuncAttrNothrow
        FuncAttrProperty
        FuncAttrRef
        FuncAttrReturn
        FuncAttrScope
        FuncAttrTrusted
        FuncAttrSafe

    FuncAttrPure:
        Na

    FuncAttrNothrow:
        Nb

    FuncAttrRef:
        Nc

    FuncAttrProperty:
        Nd

    FuncAttrTrusted:
        Ne

    FuncAttrSafe:
        Nf

    FuncAttrNogc:
        Ni

    FuncAttrReturn:
        Nj

    FuncAttrScope:
        Nl

    Arguments:
        Argument
        Argument Arguments

    Argument:
        Argument2
        M Argument2     // scope

    Argument2:
        Type
        J Type     // out
        K Type     // ref
        L Type     // lazy

    ArgClose
        X     // variadic T t,...) style
        Y     // variadic T t...) style
        Z     // not variadic
    */
    void parseCallConvention()
    {
        // CallConvention
        switch ( front )
        {
        case 'F': // D
            popFront();
            break;
        case 'U': // C
            popFront();
            put( "extern (C) " );
            break;
        case 'W': // Windows
            popFront();
            put( "extern (Windows) " );
            break;
        case 'V': // Pascal
            popFront();
            put( "extern (Pascal) " );
            break;
        case 'R': // C++
            popFront();
            put( "extern (C++) " );
            break;
        default:
            error();
        }
    }

    void parseModifier()
    {
        switch ( front )
        {
        case 'y':
            popFront();
            put( "immutable " );
            break;
        case 'O':
            popFront();
            put( "shared " );
            if ( front == 'x' )
                goto case 'x';
            if ( front == 'N' )
                goto case 'N';
            break;
        case 'N':
            if ( peek( 1 ) != 'g' )
                break;
            popFront();
            popFront();
            put( "inout " );
            if ( front == 'x' )
                goto case 'x';
            break;
        case 'x':
            popFront();
            put( "const " );
            break;
        default: break;
        }
    }

    void parseFuncAttr()
    {
        // FuncAttrs
        breakFuncAttrs:
        while ('N' == front)
        {
            popFront();
            switch ( front )
            {
            case 'a': // FuncAttrPure
                popFront();
                put( "pure " );
                continue;
            case 'b': // FuncAttrNoThrow
                popFront();
                put( "nothrow " );
                continue;
            case 'c': // FuncAttrRef
                popFront();
                put( "ref " );
                continue;
            case 'd': // FuncAttrProperty
                popFront();
                put( "@property " );
                continue;
            case 'e': // FuncAttrTrusted
                popFront();
                put( "@trusted " );
                continue;
            case 'f': // FuncAttrSafe
                popFront();
                put( "@safe " );
                continue;
            case 'g':
            case 'h':
            case 'k':
                // NOTE: The inout parameter type is represented as "Ng".
                //       The vector parameter type is represented as "Nh".
                //       The return parameter type is represented as "Nk".
                //       These make it look like a FuncAttr, but infact
                //       if we see these, then we know we're really in
                //       the parameter list.  Rewind and break.
                pos--;
                break breakFuncAttrs;
            case 'i': // FuncAttrNogc
                popFront();
                put( "@nogc " );
                continue;
            case 'j': // FuncAttrReturn
                popFront();
                put( "return " );
                continue;
            case 'l': // FuncAttrScope
                popFront();
                put( "scope " );
                continue;
            case 'm': // FuncAttrLive
                popFront();
                put( "@live " );
                continue;
            default:
                error();
            }
        }
    }

    void parseFuncArguments() scope
    {
        // Arguments
        for ( size_t n = 0; true; n++ )
        {
            debug(info) printf( "tok (%c)\n", front );
            switch ( front )
            {
            case 'X': // ArgClose (variadic T t...) style)
                popFront();
                put( "..." );
                return;
            case 'Y': // ArgClose (variadic T t,...) style)
                popFront();
                put( ", ..." );
                return;
            case 'Z': // ArgClose (not variadic)
                popFront();
                return;
            default:
                break;
            }
            putComma(n);
            if ( 'M' == front )
            {
                popFront();
                put( "scope " );
            }
            if ( 'N' == front )
            {
                popFront();
                if ( 'k' == front ) // Return (Nk Parameter2)
                {
                    popFront();
                    put( "return " );
                }
                else
                    pos--;
            }
            switch ( front )
            {
            case 'I': // in  (I Type)
                popFront();
                put("in ");
                if (front == 'K')
                    goto case;
                parseType();
                continue;
            case 'K': // ref (K Type)
                popFront();
                put( "ref " );
                parseType();
                continue;
            case 'J': // out (J Type)
                popFront();
                put( "out " );
                parseType();
                continue;
            case 'L': // lazy (L Type)
                popFront();
                put( "lazy " );
                parseType();
                continue;
            default:
                parseType();
            }
        }
    }

    enum IsDelegate { no, yes }

    /*
        TypeFunction:
            CallConvention FuncAttrs Arguments ArgClose Type
    */
    char[] parseTypeFunction( char[] name = null, IsDelegate isdg = IsDelegate.no ) return
    {
        debug(trace) printf( "parseTypeFunction+\n" );
        debug(trace) scope(success) printf( "parseTypeFunction-\n" );
        auto beg = len;

        parseCallConvention();
        auto attrbeg = len;
        parseFuncAttr();

        auto argbeg = len;
        put( '(' );
        parseFuncArguments();
        put( ')' );
        if (attrbeg < argbeg)
        {
            // move function attributes behind arguments
            shift( dst[argbeg - 1 .. argbeg] ); // trailing space
            shift( dst[attrbeg .. argbeg - 1] ); // attributes
            argbeg = attrbeg;
        }
        auto retbeg = len;
        parseType();
        put( ' ' );
        // append name/delegate/function
        if ( name.length )
        {
            if ( !contains( dst[0 .. len], name ) )
                put( name );
            else if ( shift( name ).ptr != name.ptr )
            {
                argbeg -= name.length;
                retbeg -= name.length;
            }
        }
        else if ( IsDelegate.yes == isdg )
            put( "delegate" );
        else
            put( "function" );
        // move arguments and attributes behind name
        shift( dst[argbeg .. retbeg] );
        return dst[beg..len];
    }

    static bool isCallConvention( char ch )
    {
        switch ( ch )
        {
            case 'F', 'U', 'V', 'W', 'R':
                return true;
            default:
                return false;
        }
    }

    /*
    Value:
        n
        Number
        i Number
        N Number
        e HexFloat
        c HexFloat c HexFloat
        A Number Value...

    HexFloat:
        NAN
        INF
        NINF
        N HexDigits P Exponent
        HexDigits P Exponent

    Exponent:
        N Number
        Number

    HexDigits:
        HexDigit
        HexDigit HexDigits

    HexDigit:
        Digit
        A
        B
        C
        D
        E
        F
    */
    void parseValue(scope  char[] name = null, char type = '\0' ) scope
    {
        debug(trace) printf( "parseValue+\n" );
        debug(trace) scope(success) printf( "parseValue-\n" );

//        printf( "*** %c\n", front );
        switch ( front )
        {
        case 'n':
            popFront();
            put( "null" );
            return;
        case 'i':
            popFront();
            if ( '0' > front || '9' < front )
                error( "Number expected" );
            goto case;
        case '0': .. case '9':
            parseIntegerValue( name, type );
            return;
        case 'N':
            popFront();
            put( '-' );
            parseIntegerValue( name, type );
            return;
        case 'e':
            popFront();
            parseReal();
            return;
        case 'c':
            popFront();
            parseReal();
            put( '+' );
            match( 'c' );
            parseReal();
            put( 'i' );
            return;
        case 'a': case 'w': case 'd':
            char t = front;
            popFront();
            auto n = decodeNumber();
            match( '_' );
            put( '"' );
            foreach (i; 0..n)
            {
                auto a = ascii2hex( front ); popFront();
                auto b = ascii2hex( front ); popFront();
                auto v = cast(char)((a << 4) | b);
                if (' ' <= v && v <= '~')   // ASCII printable
                {
                    put(v);
                }
                else
                {
                    put("\\x");
                    putAsHex(v, 2);
                }
            }
            put( '"' );
            if ( 'a' != t )
                put(t);
            return;
        case 'A':
            // NOTE: This is kind of a hack.  An associative array literal
            //       [1:2, 3:4] is represented as HiiA2i1i2i3i4, so the type
            //       is "Hii" and the value is "A2i1i2i3i4".  Thus the only
            //       way to determine that this is an AA value rather than an
            //       array value is for the caller to supply the type char.
            //       Hopefully, this will change so that the value is
            //       "H2i1i2i3i4", rendering this unnecesary.
            if ( 'H' == type )
                goto LassocArray;
            // A Number Value...
            // An array literal. Value is repeated Number times.
            popFront();
            put( '[' );
            auto n = decodeNumber();
            foreach ( i; 0 .. n )
            {
                putComma(i);
                parseValue();
            }
            put( ']' );
            return;
        case 'H':
        LassocArray:
            // H Number Value...
            // An associative array literal. Value is repeated 2*Number times.
            popFront();
            put( '[' );
            auto n = decodeNumber();
            foreach ( i; 0 .. n )
            {
                putComma(i);
                parseValue();
                put(':');
                parseValue();
            }
            put( ']' );
            return;
        case 'S':
            // S Number Value...
            // A struct literal. Value is repeated Number times.
            popFront();
            if ( name.length )
                put( name );
            put( '(' );
            auto n = decodeNumber();
            foreach ( i; 0 .. n )
            {
                putComma(i);
                parseValue();
            }
            put( ')' );
            return;
        default:
            error();
        }
    }


    void parseIntegerValue( scope char[] name = null, char type = '\0' ) scope
    {
        debug(trace) printf( "parseIntegerValue+\n" );
        debug(trace) scope(success) printf( "parseIntegerValue-\n" );

        switch ( type )
        {
        case 'a': // char
        case 'u': // wchar
        case 'w': // dchar
        {
            auto val = sliceNumber();
            auto num = decodeNumber( val );

            switch ( num )
            {
            case '\'':
                put( "'\\''" );
                return;
            // \", \?
            case '\\':
                put( "'\\\\'" );
                return;
            case '\a':
                put( "'\\a'" );
                return;
            case '\b':
                put( "'\\b'" );
                return;
            case '\f':
                put( "'\\f'" );
                return;
            case '\n':
                put( "'\\n'" );
                return;
            case '\r':
                put( "'\\r'" );
                return;
            case '\t':
                put( "'\\t'" );
                return;
            case '\v':
                put( "'\\v'" );
                return;
            default:
                switch ( type )
                {
                case 'a':
                    if ( num >= 0x20 && num < 0x7F )
                    {
                        put( '\'' );
                        put( cast(char)num );
                        put( '\'' );
                        return;
                    }
                    put( "\\x" );
                    putAsHex( num, 2 );
                    return;
                case 'u':
                    put( "'\\u" );
                    putAsHex( num, 4 );
                    put( '\'' );
                    return;
                case 'w':
                    put( "'\\U" );
                    putAsHex( num, 8 );
                    put( '\'' );
                    return;
                default:
                    assert( 0 );
                }
            }
        }
        case 'b': // bool
            put( decodeNumber() ? "true" : "false" );
            return;
        case 'h', 't', 'k': // ubyte, ushort, uint
            put( sliceNumber() );
            put( 'u' );
            return;
        case 'l': // long
            put( sliceNumber() );
            put( 'L' );
            return;
        case 'm': // ulong
            put( sliceNumber() );
            put( "uL" );
            return;
        default:
            put( sliceNumber() );
            return;
        }
    }


    /*
    TemplateArgs:
        TemplateArg
        TemplateArg TemplateArgs

    TemplateArg:
        TemplateArgX
        H TemplateArgX

    TemplateArgX:
        T Type
        V Type Value
        S Number_opt QualifiedName
        X ExternallyMangledName
    */
    void parseTemplateArgs() scope
    {
        debug(trace) printf( "parseTemplateArgs+\n" );
        debug(trace) scope(success) printf( "parseTemplateArgs-\n" );

    L_nextArg:
        for ( size_t n = 0; true; n++ )
        {
            if ( front == 'H' )
                popFront();

            switch ( front )
            {
            case 'T':
                popFront();
                putComma(n);
                parseType();
                continue;
            case 'V':
                popFront();
                putComma(n);
                // NOTE: In the few instances where the type is actually
                //       desired in the output it should precede the value
                //       generated by parseValue, so it is safe to simply
                //       decrement len and let put/append do its thing.
                char t = front; // peek at type for parseValue
                if ( t == 'Q' )
                    t = peekBackref();
                char[] name;
                // silent( delegate void() { name = parseType(); } );
                // CHANGE: copy from original silent function - due to the delegate closure
                auto nn = len; name = parseType(); len = nn;
                parseValue( name, t );
                continue;
            case 'S':
                popFront();
                putComma(n);

                if ( mayBeMangledNameArg() )
                {
                    auto l = len;
                    auto p = pos;
                    auto b = brp;
                    try
                    {
                        debug(trace) printf( "may be mangled name arg\n" );
                        parseMangledNameArg();
                        continue;
                    }
                    catch ( ParseException e )
                    {
                        len = l;
                        pos = p;
                        brp = b;
                        debug(trace) printf( "not a mangled name arg\n" );
                    }
                }
                if ( isDigit( front ) && isDigit( peek( 1 ) ) )
                {
                    // ambiguity: length followed by qualified name (starting with number)
                    // try all possible pairs of numbers
                    auto qlen = decodeNumber() / 10; // last digit needed for QualifiedName
                    pos--;
                    auto l = len;
                    auto p = pos;
                    auto b = brp;
                    while ( qlen > 0 )
                    {
                        try
                        {
                            parseQualifiedName();
                            if ( pos == p + qlen )
                                continue L_nextArg;
                        }
                        catch ( ParseException e )
                        {
                        }
                        qlen /= 10; // retry with one digit less
                        pos = --p;
                        len = l;
                        brp = b;
                    }
                }
                parseQualifiedName();
                continue;
            case 'X':
                popFront();
                putComma(n);
                parseLName();
                continue;
            default:
                return;
            }
        }
    }


    bool mayBeMangledNameArg()
    {
        debug(trace) printf( "mayBeMangledNameArg+\n" );
        debug(trace) scope(success) printf( "mayBeMangledNameArg-\n" );

        auto p = pos;
        scope(exit) pos = p;
        if ( isDigit( buf[pos] ) )
        {
            auto n = decodeNumber();
            return n >= 4 &&
                pos < buf.length && '_' == buf[pos++] &&
                pos < buf.length && 'D' == buf[pos++] &&
                isDigit( buf[pos] );
        }
        else
        {
            return pos < buf.length && '_' == buf[pos++] &&
                   pos < buf.length && 'D' == buf[pos++] &&
                   isSymbolNameFront();
        }
    }


    void parseMangledNameArg()
    {
        debug(trace) printf( "parseMangledNameArg+\n" );
        debug(trace) scope(success) printf( "parseMangledNameArg-\n" );

        size_t n = 0;
        if ( isDigit( front ) )
            n = decodeNumber();
        parseMangledName( false, n );
    }


    /*
    TemplateInstanceName:
        Number __T LName TemplateArgs Z
    */
    void parseTemplateInstanceName(bool hasNumber) scope
    {
        debug(trace) printf( "parseTemplateInstanceName+\n" );
        debug(trace) scope(success) printf( "parseTemplateInstanceName-\n" );

        auto sav = pos;
        auto saveBrp = brp;
        scope(failure)
        {
            pos = sav;
            brp = saveBrp;
        }
        auto n = hasNumber ? decodeNumber() : 0;
        auto beg = pos;
        match( "__T" );
        parseLName();
        put( "!(" );
        parseTemplateArgs();
        match( 'Z' );
        if ( hasNumber && pos - beg != n )
            error( "Template name length mismatch" );
        put( ')' );
    }


    bool mayBeTemplateInstanceName() scope
    {
        debug(trace) printf( "mayBeTemplateInstanceName+\n" );
        debug(trace) scope(success) printf( "mayBeTemplateInstanceName-\n" );

        auto p = pos;
        scope(exit) pos = p;
        auto n = decodeNumber();
        return n >= 5 &&
               pos < buf.length && '_' == buf[pos++] &&
               pos < buf.length && '_' == buf[pos++] &&
               pos < buf.length && 'T' == buf[pos++];
    }


    /*
    SymbolName:
        LName
        TemplateInstanceName
    */
    void parseSymbolName() scope
    {
        debug(trace) printf( "parseSymbolName+\n" );
        debug(trace) scope(success) printf( "parseSymbolName-\n" );

        // LName -> Number
        // TemplateInstanceName -> Number "__T"
        switch ( front )
        {
        case '_':
            // no length encoding for templates for new mangling
            parseTemplateInstanceName(false);
            return;

        case '0': .. case '9':
            if ( mayBeTemplateInstanceName() )
            {
                auto t = len;

                try
                {
                    debug(trace) printf( "may be template instance name\n" );
                    parseTemplateInstanceName(true);
                    return;
                }
                catch ( ParseException e )
                {
                    debug(trace) printf( "not a template instance name\n" );
                    len = t;
                }
            }
            goto case;
        case 'Q':
            parseLName();
            return;
        default:
            error();
        }
    }

    // parse optional function arguments as part of a symbol name, i.e without return type
    // if keepAttr, the calling convention and function attributes are not discarded, but returned
    char[] parseFunctionTypeNoReturn( bool keepAttr = false ) return scope
    {
        // try to demangle a function, in case we are pointing to some function local
        auto prevpos = pos;
        auto prevlen = len;
        auto prevbrp = brp;

        char[] attr;
        try
        {
            if ( 'M' == front )
            {
                // do not emit "needs this"
                popFront();
                parseModifier();
            }
            if ( isCallConvention( front ) )
            {
                // we don't want calling convention and attributes in the qualified name
                parseCallConvention();
                parseFuncAttr();
                if ( keepAttr )
                {
                    attr = dst[prevlen .. len];
                }
                else
                {
                    len = prevlen;
                }

                put( '(' );
                parseFuncArguments();
                put( ')' );
            }
        }
        catch ( ParseException )
        {
            // not part of a qualified name, so back up
            pos = prevpos;
            len = prevlen;
            brp = prevbrp;
            attr = null;
        }
        return attr;
    }

    /*
    QualifiedName:
        SymbolName
        SymbolName QualifiedName
    */
    char[] parseQualifiedName() return scope
    {
        debug(trace) printf( "parseQualifiedName+\n" );
        debug(trace) scope(success) printf( "parseQualifiedName-\n" );
        size_t  beg = len;
        size_t  n   = 0;

        do
        {
            if ( n++ )
                put( '.' );
            parseSymbolName();
            parseFunctionTypeNoReturn();

        } while ( isSymbolNameFront() );
        return dst[beg .. len];
    }


    /*
    MangledName:
        _D QualifiedName Type
        _D QualifiedName M Type
    */
    void parseMangledName( bool displayType, size_t n = 0 ) scope @nogc
    {
        debug(trace) printf( "parseMangledName+\n" );
        debug(trace) scope(success) printf( "parseMangledName-\n" );
        char[] name = null;

        auto end = pos + n;

        eat( '_' );
        match( 'D' );
        do
        {
            size_t  beg = len;
            size_t  nameEnd = len;
            char[] attr;
            do
            {
                if ( attr )
                    remove( attr ); // dump attributes of parent symbols
                if ( beg != len )
                    put( '.' );
                parseSymbolName();
                nameEnd = len;
                attr = parseFunctionTypeNoReturn( displayType );

            } while ( isSymbolNameFront() );

            if ( displayType )
            {
                attr = shift( attr );
                nameEnd = len - attr.length;  // name includes function arguments
            }
            name = dst[beg .. nameEnd];

            debug(info) printf( "name (%.*s)\n", cast(int) name.length, name.ptr );
            if ( 'M' == front )
                popFront(); // has 'this' pointer

            auto lastlen = len;
            auto type = parseType();
            if ( displayType )
            {
                if ( type.length )
                    put( ' ' );
                // sort (name,attr,type) -> (attr,type,name)
                shift( name );
            }
            else
            {
                // remove type
                assert( attr.length == 0 );
                len = lastlen;
            }
            if ( pos >= buf.length || (n != 0 && pos >= end) )
                return;

            switch ( front )
            {
            case 'T': // terminators when used as template alias parameter
            case 'V':
            case 'S':
            case 'Z':
                return;
            default:
            }
            put( '.' );

        } while ( true );
    }

    void parseMangledName()
    {
        parseMangledName( AddType.yes == addType );
    }

    char[] copyInput() return scope @nogc
    {
        if (dst.length < buf.length)
            // dst.length = buf.length;
            assert(0, "out of range"); // CHANGED: we pass slice of static array anyway
        char[] r = dst[0 .. buf.length];
        r[] = buf[];
        return r;
    }

    char[] doDemangle(alias FUNC)() return scope @nogc
    {
        while ( true )
        {
            try
            {
                debug(info) printf( "demangle(%.*s)\n", cast(int) buf.length, buf.ptr );
                FUNC();
                return dst[0 .. len];
            }
            catch ( OverflowException e )
            {
                // debug(trace) printf( "overflow... restarting\n" );
                // auto a = minBufSize;
                // auto b = 2 * dst.length;
                // auto newsz = a < b ? b : a;
                // debug(info) printf( "growing dst to %lu bytes\n", newsz );
                // dst.length = newsz;
                // pos = len = brp = 0;
                // continue;
                assert(0, "owerflow"); // CHANGE: we can't change length of static array provided as dst
            }
            catch ( ParseException e )
            {
                debug(info)
                {
                    auto msg = e.toString();
                    printf( "error: %.*s\n", cast(int) msg.length, msg.ptr );
                }
                return copyInput();
            }
            catch ( Exception e )
            {
                assert( false ); // no other exceptions thrown
            }
        }
    }

    char[] demangleName() nothrow @nogc
    {
        return doDemangle!parseMangledName();
    }

    char[] demangleType() nothrow
    {
        return doDemangle!parseType();
    }
}

/**
 * Demangles D mangled names.  If it is not a D mangled name, it returns its
 * argument name.
 *
 * Params:
 *  buf = The string to demangle.
 *  dst = An optional destination buffer.
 *
 * Returns:
 *  The demangled name or the original string if the name is not a mangled D
 *  name.
 */
char[] demangle( const(char)[] buf, char[] dst = null ) nothrow pure @safe @nogc
{
    auto d = Demangle!()(buf, dst);
    // fast path (avoiding throwing & catching exception) for obvious
    // non-D mangled names
    if (buf.length < 2 || !(buf[0] == 'D' || buf[0..2] == "_D"))
        return d.copyInput();
    return d.demangleName();
}

// locally purified for internal use here only
extern (C) private
{
    pure @trusted @nogc nothrow pragma(mangle, "fakePureReprintReal") void pureReprintReal(char[] nptr);

    void fakePureReprintReal(char[] nptr)
    {
        import core.stdc.stdlib : strtold;
        import core.stdc.stdio : snprintf;
        import core.stdc.errno : errno;

        const err = errno;
        real val = strtold(nptr.ptr, null);
        snprintf(nptr.ptr, nptr.length, "%#Lg", val);
        errno = err;
    }
}
