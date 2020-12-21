module stringify.internal.memory;

import std.traits : hasIndirections, isDelegate, isFunctionPointer, isPointer, PointerTarget;

version (D_Exceptions)
{
    import core.exception : onOutOfMemoryError;
    private enum allocationFailed = `onOutOfMemoryError();`;
}
else
{
    private enum allocationFailed = `assert(0, "Memory allocation failed");`;
}

/**
 * Allocates class or struct on the heap.
 * It automatically emplaces it to tyhe allocated memory with provided args.
 * On error, `onOutOfMemoryError` assert is called.
 *
 * When `gcRoot` is set, it also sets the memory range to be scanned by GC for pointers (off by default).
 */
auto heapAlloc(T, Args...) (Args args) @trusted
if (is(T == struct))
{
    pragma(inline);
    import std.conv : emplace;

    // allocate memory for the object
    auto memory = enforceMalloc(T.sizeof);

    // call T's constructor and emplace instance on newly allocated memory
    return emplace!(T, Args)(memory[0..T.sizeof], args);
}

/**
 * Deallocates `heapAlloc` allocated memory.
 * It automatically calls the object destructor and removes it from GC scanning (no effect if not
 * added there)
 */
void heapDealloc(T)(ref T obj) @trusted
if (isPointer!T && is(PointerTarget!T == struct))
{
    pragma(inline);
    import core.memory : GC, pureFree;
    import std.traits : hasElaborateDestructor;

    alias U = PointerTarget!T;
    static if (hasElaborateDestructor!(PointerTarget!T)) destroy(*obj);

    // free memory occupied by object
    pureFree(cast(void*)obj);
    obj = null;
}

@safe unittest
{
    struct Foo
    {
        int i;
        this(int i) { this.i = i; }
    }

    Foo* f = heapAlloc!Foo(42);
    assert(f.i == 42);
    f.heapDealloc();
    assert(f is null);
}

// NOTE: these are copy pasted from: https://github.com/dlang/phobos/blob/master/std/internal/memory.d

/+
Mnemonic for `enforce!OutOfMemoryError(malloc(size))` that (unlike malloc)
can be considered pure because it causes the program to abort if the result
of the allocation is null, with the consequence that errno will not be
visibly changed by calling this function. Note that `malloc` can also
return `null` in non-failure situations if given an argument of 0. Hence,
it is a programmer error to use this function if the requested allocation
size is logically permitted to be zero. `enforceCalloc` and `enforceRealloc`
work analogously.
All these functions are usable in `betterC`.
+/
void* enforceMalloc()(size_t size) @nogc nothrow pure @safe
{
    auto result = fakePureMalloc(size);
    if (!result) mixin(allocationFailed);
    return result;
}

// ditto
void* enforceCalloc()(size_t nmemb, size_t size) @nogc nothrow pure @safe
{
    auto result = fakePureCalloc(nmemb, size);
    if (!result) mixin(allocationFailed);
    return result;
}

// ditto
void* enforceRealloc()(void* ptr, size_t size) @nogc nothrow pure @system
{
    auto result = fakePureRealloc(ptr, size);
    if (!result) mixin(allocationFailed);
    return result;
}

// Purified for local use only.
extern (C) @nogc nothrow pure private
{
    pragma(mangle, "malloc") void* fakePureMalloc(size_t) @safe;
    pragma(mangle, "calloc") void* fakePureCalloc(size_t nmemb, size_t size) @safe;
    pragma(mangle, "realloc") void* fakePureRealloc(void* ptr, size_t size) @system;
}
