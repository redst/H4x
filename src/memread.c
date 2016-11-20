#include <sys/uio.h>

void memread(pid_t pid, void *targetaddr, void *ret, ssize_t sz)
{
    struct iovec data = {
        targetaddr,
        sz
    }, local = {
        ret,
        sz
    };
    process_vm_readv(pid, &local, 1, &data, 1, 0);
}

void memwrite(pid_t pid, void *targetaddr, void *ret, ssize_t sz)
{
    struct iovec data = {
        targetaddr,
        sz
    }, local = {
        ret,
        sz
    };
    process_vm_writev(pid, &local, 1, &data, 1, 0);
}

