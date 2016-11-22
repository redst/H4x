#define _GNU_SOURCE
#include <string.h>
#include <sys/uio.h>

#define N UIO_MAXIOV

#define INCREASE(n,m)  ((n*1.5 < m ? n*1.5 : m))

void memread(pid_t pid, void *targetaddr, void *ret, ssize_t sz)
{
    ssize_t n;
    struct iovec data = {
        targetaddr,
        sz
    }, local = {
        ret,
        sz
    };
    n = process_vm_readv(pid, &local, 1, &data, 1, 0);
    if (n > 0 && n < sz)
        memread(pid, targetaddr+n, ret+n, sz-n);
}

void memwrite(pid_t pid, void *targetaddr, void *ret, ssize_t sz)
{
    ssize_t n;
    struct iovec data = {
        targetaddr,
        sz
    }, local = {
        ret,
        sz
    };
    n = process_vm_writev(pid, &local, 1, &data, 1, 0);
    if (n > 0 && n < sz)
        memwrite(pid, targetaddr+n, ret+n, sz-n);
}

ssize_t memread0(pid_t pid, void *targetaddr, void *ret, ssize_t sz, void *del, ssize_t elemsz)
{
    ssize_t n = 64 < sz ? 64 : sz;
    ssize_t j, red=0;
    void *ptr = ret;

    do {
        memread(pid, targetaddr+red, ret+red, n);
        for(j=0; j<n; j+=elemsz) {
            if (!memcmp(ret+red+j, del, elemsz))
                return red+j;
        }
        red+=j;
        n = INCREASE(n, sz-red);
    } while (red<=sz-elemsz);
    return -red;
}

