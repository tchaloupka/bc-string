module bc.core.intrinsics;

version(LDC)
{
    ///
    public import ldc.intrinsics: _expect = llvm_expect;
}
else version(GNU)
{
    import gcc.builtins: __builtin_expect, __builtin_clong;

    ///
    T _expect(T)(in T val, in T expected_val) if (__traits(isIntegral, T))
    {
        static if (T.sizeof <= __builtin_clong.sizeof)
            return cast(T) __builtin_expect(val, expected_val);
        else
            return val;
    }
}
else
{
    ///
    T _expect(T)(T val, T expected_val) if (__traits(isIntegral, T))
    {
        return val;
    }
}

// Workarounds for betterC
version (D_BetterC)
{
    pragma(mangle, "_D4core8lifetime16testEmplaceChunkFNaNbNiNfAvmmZv")
    nothrow @nogc @safe pure void testEmplaceChunk(void[] chunk, size_t typeSize, size_t typeAlignment) {}

    version (DigitalMars)
    {
        extern (C)
        nothrow @nogc
        short* _memset16(short *p, short value, size_t count)
        {
            short *pstart = p;
            short *ptop;

            for (ptop = &p[count]; p < ptop; p++)
                *p = value;
            return pstart;
        }
    }

    version (assert)
    {
        version (LDC)
        {
            // See: https://github.com/ldc-developers/ldc/issues/2425
            // See: https://forum.dlang.org/post/heksucpdamkgwnztyitr@forum.dlang.org
            extern(C)
            nothrow @nogc
            void _d_array_slice_copy(void* dst, size_t dstlen, void* src, size_t srclen, size_t elemsz)
            {
                import ldc.intrinsics : llvm_memcpy;
                llvm_memcpy!size_t(dst, src, dstlen * elemsz, 0);
            }
        }
    }
}
