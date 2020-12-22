module bc.core.system.backtrace;

version (D_BetterC) {}
else:

/**
 * This struct us used to mimic private class in https://github.com/dlang/druntime/blob/master/src/core/runtime.d#L734
 * so we can access callstack.
 */
struct TraceInfo
{
    // class fields
    void*    _vtbl;
    void*    _monitor;
    void*    _interface;  // introduced in DMD 2.071

    // make sure the ABI matches
    static assert (
        {static interface I {} static class C: I {} return __traits(classInstanceSize, C);}() == (void*[3]).sizeof
    );

    // actual fields we care about
    static enum MAXFRAMES = 128;
    int        numframes;
    void*[MAXFRAMES] callstack;

    @property void*[] frames() return nothrow @trusted @nogc {
        return callstack.ptr[0 .. numframes];
    }

    // NOTE: The first few frames with the current implementation are
    //       inside core.runtime and the object code, so eliminate
    //       these for readability.  The alternative would be to
    //       exclude the first N frames that are in a list of
    //       mangled function names.
    version (LDC) enum FIRSTFRAME = 0; // See: https://github.com/ldc-developers/druntime/blob/ldc/src/core/runtime.d#L861
    else {
        static if (__VERSION__ < 2092) enum FIRSTFRAME = 4;
        else enum FIRSTFRAME = 5;
    }
    // enum FIRSTFRAME = 0;

    version (Posix)
    {
        /// Gather trace info from Throwable
        this(Throwable ex) nothrow @trusted @nogc pure
        {
            this(ex.info);
        }

        this(Throwable.TraceInfo ti, uint first = FIRSTFRAME) nothrow @trusted @nogc pure
        {
            if (ti !is null)
            {
                auto obj = cast(Object)ti;

                // this can change in druntime
                assert(typeid(obj).name == "core.runtime.DefaultTraceInfo", "Unexpected trace info type");

                auto trace = cast(TraceInfo*)(cast(void*)obj);
                if (trace.numframes >= first)
                {
                    this.numframes = trace.numframes - first;
                    this.callstack[0..numframes] = trace.callstack[first..trace.numframes];
                }
            }
        }
    }

    /// Gets current trace info
    static TraceInfo current()() nothrow @trusted @nogc
    {
        version (Posix)
        {
            import bc.core.system.linux.execinfo : backtrace, thread_stackBottom;

            // just a copy from: https://github.com/dlang/druntime/blob/master/src/core/runtime.d#L742
            // again, cant't use directly as it's not @nogc

            // it may not be 1 but it is good enough to get
            // in CALL instruction address range for backtrace
            enum CALL_INSTRUCTION_SIZE = 1;

            TraceInfo ret;

            static if (__traits(compiles, backtrace((void**).init, int.init)))
                ret.numframes = backtrace(ret.callstack.ptr, MAXFRAMES);
            // Backtrace succeeded, adjust the frame to point to the caller
            if (ret.numframes >= 2)
                foreach (ref elem; ret.callstack)
                    elem -= CALL_INSTRUCTION_SIZE;
            else // backtrace() failed, do it ourselves
            {
                static void** getBasePtr() nothrow @nogc
                {
                    version (D_InlineAsm_X86)
                        asm nothrow @nogc { naked; mov EAX, EBP; ret; }
                    else
                        version (D_InlineAsm_X86_64)
                            asm nothrow @nogc { naked; mov RAX, RBP; ret; }
                    else
                        return null;
                }

                auto  stackTop    = getBasePtr();
                auto  stackBottom = cast(void**)thread_stackBottom();
                void* dummy;

                if (stackTop && &dummy < stackTop && stackTop < stackBottom)
                {
                    auto stackPtr = stackTop;

                    for (ret.numframes = 0; stackTop <= stackPtr && stackPtr < stackBottom && ret.numframes < MAXFRAMES; )
                    {
                        ret.callstack[ret.numframes++] = *(stackPtr + 1) - CALL_INSTRUCTION_SIZE;
                        stackPtr = cast(void**) *stackPtr;
                    }
                }
            }
        }
        else static assert(0, "Unsupported platform");

        if (ret.numframes > 1)
        {
            // drop first frame as it points to this method
            import std.algorithm : copy;
            ret.numframes--;
            ret.callstack[1..ret.numframes+1].copy(ret.callstack[0..ret.numframes]);
        }

        return ret;
    }

    /// Dumps trace info to the provided sink.
    /// Returns: size of written data
    size_t dumpTo(S)(ref S sink) nothrow @nogc @trusted // TODO: well..
    {
        if (numframes)
        {
            version (Posix)
            {
                import bc.core.system.linux.dwarf : dumpCallstack;
                import bc.core.system.linux.elf : Image;
                import bc.core.system.linux.execinfo : backtrace_symbols;
                import core.sys.posix.stdlib : free;

                const char** frameList = () @trusted { return backtrace_symbols(&callstack[0], cast(int) numframes); }();
                scope(exit) () @trusted { free(cast(void*)frameList); }();

                auto image = Image.openSelf();
                if (image.isValid)
                    return image.processDebugLineSectionData(sink, callstack[0..numframes], frameList, &dumpCallstack!S);

                return dumpCallstack(sink, image, callstack[0..numframes], frameList, null);
            }
            else static assert(0, "Unsupported platform");
        }
        return 0;
    }
}

version (Posix)
{
    // get current callstack
    unittest
    {
        import bc.string.string : String;

        auto ti = TraceInfo.current();
        String buf, buf2;
        immutable ret = ti.dumpTo(buf);
        assert(ret == buf.length);

        // import std.stdio : writeln;
        // writeln(cast(const(char)[])buf[]);
    }

    // get callstack from defaultTraceHandler
    unittest
    {
        import bc.string.string : String;
        import core.runtime : defaultTraceHandler;

        auto dti = defaultTraceHandler();
        assert(dti !is null);

        String buf, buf2;
        auto ti = TraceInfo(dti);
        immutable ret = ti.dumpTo(buf);
        assert(ret == buf.length);

        foreach (ln; dti) { buf2 ~= ln; buf2 ~= '\n'; }

        // import std.stdio : writeln;
        // writeln("our: ", cast(const(char)[])buf[]);
        // writeln("-----------------");
        // writeln("orig: ", cast(const(char)[])buf2[]);
        assert(buf[] == buf2[0..$-1]);
    }
}
