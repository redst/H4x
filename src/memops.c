#define _GNU_SOURCE
#include <string.h>
#include <sys/uio.h>
#include <errno.h>
#include <stdio.h>
#include <unistd.h>
#include <limits.h>
#include <sys/user.h>

#define N UIO_MAXIOV

#define INCREASE(n,m)  ((n*1.5 < m ? n*1.5 : m))

ssize_t memread(pid_t pid, void *targetaddr, void *ret, ssize_t sz)
{
    ssize_t n,m;
    struct iovec data = {
        targetaddr,
        sz
    }, local = {
        ret,
        sz
    };
    n = process_vm_readv(pid, &local, 1, &data, 1, 0);
    if (n > 0 && n < sz) {
        m = memread(pid, targetaddr+n, ret+n, sz-n);
        if (m < 0)
            return errno;
        else
            return n+m;
    }
    else if (n < 0) {
        return errno;
    }
    else {
        return n;
    }
}

ssize_t memwrite(pid_t pid, void *targetaddr, void *ret, ssize_t sz)
{
    ssize_t n, m;
    struct iovec data = {
        targetaddr,
        sz
    }, local = {
        ret,
        sz
    };
    n = process_vm_writev(pid, &local, 1, &data, 1, 0);
    if (n > 0 && n < sz) {
        m = memwrite(pid, targetaddr+n, ret+n, sz-n);
        if (m < 0)
            return errno;
        else
            return n+m;
    }
    else if (n < 0) {
        return errno;
    }
    else {
        return n;
    }
}

ssize_t memread0(pid_t pid, void *targetaddr, void *ret, ssize_t sz, void *del, 
                 ssize_t elemsz)
{
    ssize_t n = 64 < sz ? 64 : sz;
    ssize_t j, red=0;
    void *ptr = ret;

    do {
        if (memread(pid, targetaddr+red, ret+red, n) < 0)
            return 0;
        for(j=0; j<n; j+=elemsz) {
            if (!memcmp(ret+red+j, del, elemsz))
                return red+j;
        }
        red+=j;
        n = INCREASE(n, sz-red);
    } while (red<=sz-elemsz);
    return sz+n;
}

void f(size_t n)
{
    printf("size: 0x%x\n", n);
    char buff[n];
    printf("size: 0x%x\n", n);
}

int proc_regions(pid_t pid, size_t addrs[][2])
{
    char filename[1024];
    char lines[1024];
    sprintf(filename, "/proc/%d/maps", pid);
    FILE *fd = fopen(filename, "r");
    int n = 0;
    while (NULL != fgets(lines, 256, fd)) {
        size_t add0,add1,tmp;
        if (sscanf(lines, "%x-%x r%*[-rwxp] %x", &add0, &add1, &tmp) > 2) {
            if (n != 0 && addrs[n-1][0] + addrs[n-1][1] == add0) {
                addrs[n-1][1] += add1 - add0;
            }
            else {
                addrs[n][0] = add0;
                addrs[n][1] = add1-add0;
                n++;
            } 
        }
    }
    fclose(fd);
    return n;
}

int cmp_bytes(pid_t pid, 
              size_t regions[][2], 
              size_t n_regs,
              int (*cmp_f)(char *),
              size_t size, 
              size_t step, 
              size_t *ret)
{
    int i, pag, j, n = 0;
    char buff[PAGE_SIZE + size];
    struct iovec local = {
        .iov_base = buff,
        .iov_len = PAGE_SIZE + size
    }, remote = {
        .iov_base = 0,
        .iov_len = PAGE_SIZE + size
    };
    for (i = 0; i < n_regs; i++) {
        for (pag = 0; pag < (regions[i][1] + PAGE_SIZE -1)/PAGE_SIZE; pag++) {
            remote.iov_base = (void *) regions[i][0] + PAGE_SIZE * pag;
            process_vm_readv(pid, &local, 1, &remote, 1, 0);
            for (j = 0; j < PAGE_SIZE; j += step) {
                if (!cmp_f(buff+j)) {
                    ret[n++] = regions[i][0] + PAGE_SIZE * pag + j;
                }
            }
        }
    }
    return n;
}

int search_bytes(pid_t pid,
                 size_t regions[][2], 
                 size_t n_regs,
                 char *val,
                 size_t size, 
                 size_t step, 
                 size_t *ret)
{
    return cmp_bytes(pid, regions, n_regs, 
#ifdef __GNUC__
                ({int _(char *arg) {
                    return memcmp(val, arg, size);
                }_;}), 
#else
                __cmp__size,
#endif
                size, step, ret);
}


// size_t rets[0x1000000];
// int main(void)
// {
//     size_t addrs[4096][2];
//     int i, n;
//     addrs[0][0] = 0x4e4c000;
//     addrs[0][1] = 4096;
//     n = proc_regions(6048, addrs);
//     for (i=0; i < n; i++) {
//         printf("addr: %p, size: 0x%x\n", addrs[i][0], addrs[i][1]);
//     }
//     char name[] = "Consecration";
//     n = search_bytes(6048, addrs, n, name, sizeof(name)-1, 1, rets);
//     for (i = 0; i < n; i++) {
//         printf("found at: %p\n", rets[i]);
//     }
//     return 0;
// }
