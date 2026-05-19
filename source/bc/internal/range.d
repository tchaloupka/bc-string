/**
 * betterC-safe variants of phobos std.range primitives.
 *
 * The autodecoding `front` in std.range.primitives pulls in std.utf, which in recent phobos
 * throws via `_d_newclassT` and therefore can't be used with -betterC. This module provides
 * non-autodecoding replacements for the primitives that would otherwise be instantiated with
 * narrow string types, and re-exports the rest of std.range unchanged.
 *
 * Outside of betterC this is just a public import of std.range.
 */
module bc.internal.range;

version (D_BetterC)
{
    public import std.range.primitives :
        back, empty, hasLength, hasSlicing, hasMobileElements,
        isBidirectionalRange, isInfinite,
        moveAt, moveBack, moveFront,
        popBack, popFront, popFrontExactly, popFrontN, put, save, walkLength,
        ElementEncodingType;
    public import std.range : chunks, generate, iota, only, recurrence, repeat, retro, take, takeExactly;

    import std.traits : isAutodecodableString;

    /// Element type of a range, without autodecoding narrow strings.
    template ElementType(R)
    {
        static if (is(typeof(R.init.front.init) T))
            alias ElementType = T;
        else static if (is(typeof(R.init[0]) T))
            alias ElementType = T;
        else
            alias ElementType = void;
    }

    /// `front` for built-in arrays (non narrow strings).
    @property ref inout(T) front(T)(return scope inout(T)[] a) @safe pure nothrow @nogc
    if (!isAutodecodableString!(T[]) && !is(T[] == void[]))
    {
        assert(a.length, "Attempting to fetch the front of an empty array of " ~ T.stringof);
        return a[0];
    }

    /// `front` for narrow strings - returns the first code unit, no autodecoding.
    /// Note: autodecoding `front` would drag in std.utf which is not -betterC compatible.
    @property T front(T)(scope const(T)[] a) @safe pure
    if (isAutodecodableString!(T[]))
    {
        assert(a.length, "Attempting to fetch the front of an empty array of " ~ T.stringof);
        return a[0];
    }

    /// Determines whether `R` is an input range. Uses this module's non-autodecoding `front`.
    enum bool isInputRange(R) =
        is(typeof(R.init) == R)
        && is(typeof((R r) { return r.empty; } (R.init)) == bool)
        && (is(typeof((return ref R r) => r.front)) || is(typeof(ref (return ref R r) => r.front)))
        && !is(typeof((R r) { return r.front; } (R.init)) == void)
        && is(typeof((R r) => r.popFront));

    /// Determines whether `R` is a forward range.
    enum bool isForwardRange(R) = isInputRange!R
        && is(typeof((R r) { return r.save; } (R.init)) == R);

    /// Determines whether `R` is a random access range.
    enum bool isRandomAccessRange(R) =
        isInputRange!R
        && (isBidirectionalRange!R || isInfinite!R)
        && is(typeof(R.init[1]))
        && !isAutodecodableString!R
        && is(typeof(R.init[1]) == ElementType!R);
}
else
{
    public import std.range;
}
