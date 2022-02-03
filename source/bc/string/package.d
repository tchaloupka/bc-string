module bc.string;

public import bc.string.ascii;
public import bc.string.format;
public import bc.string.numeric;
public import bc.string.string;

import core.stdc.stdio;

version (CI_MAIN)
{
    // workaround for dub not supporting unittests with betterC
    version (D_BetterC)
    {
        extern(C) int main()
        {
            version (unittest)
            {
                import std.meta : AliasSeq;

                alias modules = AliasSeq!(bc.string.ascii, bc.string.format, bc.string.numeric, bc.string.string);
                static foreach (m; modules)
                {
                    static foreach(u; __traits(getUnitTests, m)) {
                        static if (__traits(getAttributes, u).length)
                            printf("unittest %s:%d | '" ~ __traits(getAttributes, u)[0] ~ "'\n", __traits(getLocation, u)[0].ptr, __traits(getLocation, u)[1]);
                        else
                            printf("unittest %s:%d\n", __traits(getLocation, u)[0].ptr, __traits(getLocation, u)[1]);
                        u();
                    }
                }
                debug printf("All unit tests have been run successfully.\n");
                return 0;
            }
            else return rtTest();
        }

        // workaround for linking error: `undefined reference to `_D4core8internal5array8equality__T8__equalsTaTaZQoFNaNbNiNeMxAaMxQeZb'`
        bool equals(S1, S2)(S1 a, S2 b)
        {
            if (a.length != b.length) return false;
            import core.stdc.string : strncmp;
            return strncmp(a.ptr, b.ptr, a.length) == 0;
        }
    }
    else
    {
        int main()
        {
            version (unittest) return 0; // run automagically
            else return rtTest();
        }

        bool equals(S1, S2)(S1 a, S2 b) { return a == b; }
    }

    int rtTest()()
    {
        // just a compilation test - same as in the README to be sure it actually works..

        import bc.core.memory;
        import bc.internal.utf : byCodeUnit;
        import core.stdc.string : strlen;
        import std.algorithm : filter;
        import std.range : chunks;

        assert(nogcFormat!"abcd abcd".equals("abcd abcd"));
        assert(nogcFormat!"123456789a".equals("123456789a"));
        version (D_NoBoundsChecks) {}
        else version (D_BetterC) {}
        else
        {
            () @trusted
            {
                import core.exception : RangeError;
                import std.exception : assertThrown;
                char[5] buf;
                version (assert) assertThrown!RangeError(buf.nogcFormatTo!"123412341234");
            }();
        }

        // literal escape
        assert(nogcFormat!"123 %%".equals("123 %"));
        assert(nogcFormat!"%%%%".equals("%%"));

        // %d
        assert(nogcFormat!"%d"(1234).equals("1234"));
        assert(nogcFormat!"%4d"(42).equals("  42"));
        assert(nogcFormat!"%04d"(42).equals("0042"));
        assert(nogcFormat!"%04d"(-42).equals("-042"));
        assert(nogcFormat!"ab%dcd"(1234).equals("ab1234cd"));
        assert(nogcFormat!"ab%d%d"(1234, 56).equals("ab123456"));

        // %x
        assert(nogcFormat!"0x%x"(0x1234).equals("0x1234"));

        // %p
        assert(nogcFormat!("%p")(0x1234).equals("0000000000001234"));

        // %s
        assert(nogcFormat!"12345%s"("12345").equals("1234512345"));
        assert(nogcFormat!"12345%s"(12345).equals("1234512345"));
        enum Floop {XXX, YYY, ZZZ}
        assert(nogcFormat!"12345%s"(Floop.YYY).equals("12345YYY"));
        char[4] str = "foo\0";
        assert(nogcFormat!"%s"(&str[0]).equals("foo"));

        version (D_BetterC) {} // can't import std.uuid in betterC
        else
        {
            import std.uuid;
            assert(nogcFormat!"%s"(
                parseUUID("22390768-cced-325f-8f0f-cfeaa19d0ccd"))
                .equals("22390768-cced-325f-8f0f-cfeaa19d0ccd"));
        }

        // array format
        version (D_BetterC)
        {
            int[] arr = (cast(int*)enforceMalloc(int.sizeof*10))[0..10];
            foreach (i; 0..10) arr[i] = i;
            scope (exit) pureFree(arr.ptr);
        }
        else auto arr = [0,1,2,3,4,5,6,7,8,9];

        assert(nogcFormat!"foo %(%d %)"(arr[1..4]).equals("foo 1 2 3"));
        assert(nogcFormat!"foo %-(%d %)"(arr[1..4]).equals("foo 1 2 3"));
        assert(nogcFormat!"foo %(-%d-%|, %)"(arr[1..4]).equals("foo -1-, -2-, -3-"));
        assert(nogcFormat!"%(0x%02x %)"(arr[1..4]).equals("0x01 0x02 0x03"));
        assert(nogcFormat!"%(%(%d %)\n%)"(arr[1..$].chunks(3)).equals("1 2 3\n4 5 6\n7 8 9"));

        // range format
        auto r = arr.filter!(a => a < 5);
        assert(nogcFormat!"%s"(r).equals("[0, 1, 2, 3, 4]"));

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

        assert(nogcFormat!"Hello %s"(5).equals("Hello 5"));

        struct Foo { int x, y; }
        assert(nogcFormat!("Hello %s")(Foo(1, 2)).equals("Hello Foo(x=1, y=2)"));

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
        assert(nogcFormat!"%s"(Msg.init).equals("Msg(foo=null)"));

        RCString s = "abcd";
        assert(nogcFormat!"%s"(s).equals("abcd"));

        version (D_BetterC) {}
        else version (linux)
        {
            import bc.core.system.backtrace : TraceInfo;
            StringZ buf;
            buf.nogcFormatTo!"where am i: %s"(TraceInfo.current);
            printf("%s\n", buf.ptr);
        }

        assert(numDigits(0) == 1);
        assert(numDigits(11) == 2);
        assert(numDigits(-1) == 2);

        assert(numDigits(int.min) == 11);
        assert(numDigits(int.max) == 10);
        assert(numDigits(long.min) == 20);
        assert(numDigits(long.max) == 19);
        assert(numDigits(ulong.min) == 1);
        assert(numDigits(ulong.max) == 20);

        string str2 = "abc";

        // Intended usage
        assert(strlen(str2.tempCString()) == 3);

        // Correct usage
        auto tmp = str2.tempCString();
        assert(strlen(tmp) == 3); // or `tmp.ptr`, or `tmp.buffPtr`

        // Incorrect usage
        auto pInvalid1 = str2.tempCString().ptr;
        const char* pInvalid2 = str2.tempCString();

        RCString rcs;
        rcs ~= "fo";
        rcs ~= 'o';
        rcs ~= "bar";
        rcs ~= "baz".byCodeUnit.filter!(a => a == 'z');
        assert(rcs.length == "foobarz".length);
        assert(rcs.data.equals("foobarz"));
        assert(rcs.equals("foobarz"));
        assert(rcs.ptr == &rcs.data[0]);
        assert((rcs.ptr["foobarz".length]) == 0);

        // construction from write like params
        rcs = RCString.from("foo", 42, "bar");
        assert(rcs.equals("foo42bar"));

        auto ss = String("Hello");
        assert(ss[].equals("Hello"), s[]);
        ss ~= " String";
        assert(ss[].equals("Hello String"), ss[]);
        auto ss2 = ss.clone();
        assert(ss[].equals(ss2[]));
        assert(ss.ptr != ss2.ptr);

        auto ss3 = ss.move();
        assert(ss.length == 0);
        assert(ss3[].equals("Hello String"));

        enum indented = `
                SELECT foo, bar, baz
                    FROM foos
                    WHERE id=ANY($1) AND type_id IN (
                        SELECT id FROM bars WHERE owner=$2
                    )`;

        enum dedented =
                    "SELECT foo, bar, baz\n" ~
                    "    FROM foos\n" ~
                    "    WHERE id=ANY($1) AND type_id IN (\n" ~
                    "        SELECT id FROM bars WHERE owner=$2\n" ~
                    ")";

        // assert(dedent(indented) == dedented);

        printf("test run completed\n");
        return 0;
    }
}
