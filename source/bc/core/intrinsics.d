module bc.core.intrinsics;

version(LDC)
{
    import ldc.intrinsics: llvm_expect;
    import ldc.gccbuiltins_x86;
    public import core.simd;

    enum LDC_with_SSE42 = __traits(targetHasFeature, "sse4.2");

    // some definition aliases to commonly used names
    alias __m128i = int4;

    // some used methods aliases
    alias _expect = llvm_expect;
    alias _mm_loadu_si128 = loadUnaligned!__m128i;
    alias _mm_cmpestri = __builtin_ia32_pcmpestri128;

    // These specify the type of data that we're comparing.
    enum _SIDD_UBYTE_OPS            = 0x00;
    enum _SIDD_UWORD_OPS            = 0x01;
    enum _SIDD_SBYTE_OPS            = 0x02;
    enum _SIDD_SWORD_OPS            = 0x03;

    // These specify the type of comparison operation.
    enum _SIDD_CMP_EQUAL_ANY        = 0x00;
    enum _SIDD_CMP_RANGES           = 0x04;
    enum _SIDD_CMP_EQUAL_EACH       = 0x08;
    enum _SIDD_CMP_EQUAL_ORDERED    = 0x0c;

    // These are used in _mm_cmpXstri() to specify the return.
    enum _SIDD_LEAST_SIGNIFICANT    = 0x00;
    enum _SIDD_MOST_SIGNIFICANT     = 0x40;

    // These macros are used in _mm_cmpXstri() to specify the return.
    enum _SIDD_BIT_MASK             = 0x00;
    enum _SIDD_UNIT_MASK            = 0x40;
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

    enum LDC_with_SSE42 = false;
}
else
{
    ///
    T _expect(T)(T val, T expected_val) if (__traits(isIntegral, T))
    {
        return val;
    }

    enum LDC_with_SSE42 = false;
}

version (unittest) pragma(msg, "SSE: ", LDC_with_SSE42);

// Workarounds for betterC
version (D_BetterC)
{
    pragma(mangle, "_D4core8lifetime16testEmplaceChunkFNaNbNiNfAvmmZv")
    nothrow @nogc @safe pure void testEmplaceChunk(void[] chunk, size_t typeSize, size_t typeAlignment) {}

    version (DigitalMars)
    {
        // see: https://issues.dlang.org/show_bug.cgi?id=19946
        extern (C) nothrow @nogc:

        short* _memset16(short *p, short value, size_t count)
        {
            short *pstart = p;
            short *ptop;

            for (ptop = &p[count]; p < ptop; p++)
                *p = value;
            return pstart;
        }

        int*_memset32(int *p, int value, size_t count)
        {
            version (D_InlineAsm_X86)
            {
                asm
                {
                    mov     EDI,p           ;
                    mov     EAX,value       ;
                    mov     ECX,count       ;
                    mov     EDX,EDI         ;
                    rep                     ;
                    stosd                   ;
                    mov     EAX,EDX         ;
                }
            }
            else
            {
                int *pstart = p;
                int *ptop;

                for (ptop = &p[count]; p < ptop; p++)
                    *p = value;
                return pstart;
            }
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
