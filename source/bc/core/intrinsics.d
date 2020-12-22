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
