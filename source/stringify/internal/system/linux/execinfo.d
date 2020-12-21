module stringify.internal.system.linux.execinfo;

// Copy from (missing @nogc): https://github.com/dlang/druntime/blob/master/src/core/sys/linux/execinfo.d

version(linux):

extern (C):
nothrow:
@system:
@nogc: // missing in druntime - see https://github.com/dlang/druntime/pull/3317

int backtrace(void** buffer, int size);
char** backtrace_symbols(const(void*)* buffer, int size);
void backtrace_symbols_fd(const(void*)* buffer, int size, int fd);

// added from https://github.com/dlang/druntime/blob/master/src/core/runtime.d#L101
void* thread_stackBottom();
