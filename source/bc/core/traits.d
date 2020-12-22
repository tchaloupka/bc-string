module bc.core.traits;

/**
 * Match types like `std.typecons.Nullable` ie `mir.core.Nullable`
 */
template isStdNullable(T)
{
    import std.traits : hasMember;

    T* aggregate;

    enum bool isStdNullable =
        hasMember!(T, "isNull") &&
        hasMember!(T, "get") &&
        hasMember!(T, "nullify") &&
        is(typeof(__traits(getMember, aggregate, "isNull")()) == bool) &&
        !is(typeof(__traits(getMember, aggregate, "get")()) == void) &&
        is(typeof(__traits(getMember, aggregate, "nullify")()) == void);
}

version (D_BetterC) {}
else:

unittest
{
    import std.typecons : Nullable;
    static assert(isStdNullable!(Nullable!string));
}
