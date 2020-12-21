module stringify.internal.intrinsics;

version(LDC)
{
    public import ldc.intrinsics: _expect = llvm_expect;
}
else
{
    T _expect(T)(T val, T expected_val) if (__traits(isIntegral, T))
    {
        pragma(inline, true);
        return val;
    }
}
