module bc.core.system.linux.elf;

version (D_BetterC) {}
else:

// Copy from normally unavailable https://github.com/dlang/druntime/blob/master/src/rt/backtrace/elf.d

version (linux)
{
    import core.sys.linux.elf;
    version = LinuxOrBSD;
}
else version (FreeBSD)
{
    import core.sys.freebsd.sys.elf;
    version = LinuxOrBSD;
}
else version (DragonFlyBSD)
{
    import core.sys.dragonflybsd.sys.elf;
    version = LinuxOrBSD;
}

version (LinuxOrBSD):

import core.internal.elf.dl;
import core.internal.elf.io;

struct Image
{
    private ElfFile file;

    nothrow @nogc:

    static Image openSelf()
    {
        // see: https://github.com/dlang/druntime/commit/e3c8f38141cd148e92bc949fd88a3e50d0054807
        // const(char)* selfPath = SharedObject.thisExecutable().name().ptr;
        const(char)* selfPath;
        foreach (object; SharedObjects)
        {
            // the first object is the main binary
            selfPath = object.name().ptr;
            break;
        }

        Image image;
        if (!ElfFile.open(selfPath, image.file))
            image.file = ElfFile.init;

        return image;
    }

    @property bool isValid()
    {
        return file != ElfFile.init;
    }

    T processDebugLineSectionData(S, T)(ref S sink, const(void*)[] callstack, const char** frameList,
        scope T function(ref S, ref Image, const(void*)[], const char**, const(ubyte)[]) nothrow @nogc processor)
    {
        ElfSectionHeader dbgSectionHeader;
        ElfSection dbgSection;

        if (file.findSectionHeaderByName(".debug_line", dbgSectionHeader))
        {
            // we don't support compressed debug sections
            if (!(dbgSectionHeader.shdr.sh_flags & SHF_COMPRESSED))
                dbgSection = ElfSection(file, dbgSectionHeader);
        }

        return processor(sink, this, callstack, frameList, cast(const(ubyte)[])dbgSection.data());
    }

    @property size_t baseAddress()
    {
        // the DWARF addresses for DSOs are relative
        const isDynamicSharedObject = (file.ehdr.e_type == ET_DYN);
        if (!isDynamicSharedObject)
            return 0;

        // see: https://github.com/dlang/druntime/commit/e3c8f38141cd148e92bc949fd88a3e50d0054807
        // return cast(size_t) SharedObject.thisExecutable().baseAddress;

        size_t base = 0;
        foreach (object; SharedObjects)
        {
            // only take the first address as this will be the main binary
            base = cast(size_t) object.baseAddress;
            break;
        }

        return base;
    }
}
