#define _GNU_SOURCE
#include <string.h>
#include <sys/uio.h>
#include <errno.h>
#include <stdio.h>
#include <unistd.h>
#include <limits.h>
#include <stdint.h>
#include <stddef.h>
#include <sys/user.h>

#define MAXSIZE UIO_MAXIOV

#define UNIT ((ptrdiff_t) 1)
#define MASKN(n) (n >= sizeof(ptrdiff_t)                \
                        ? -UNIT                          \
                        : ((UNIT << 8 * n) - 1))
#define MASKEDN(n,x) (MASKN(n) & (ptrdiff_t) ((x)))

#define INCREASE(n,m)  ((n*1.5 < m ? n*1.5 : m))

#ifdef __GNUC_GNU_INLINE__
#define INLINE extern inline __attribute__((gnu_inline))
#else
#define INLINE extern inline
#endif

INLINE  uint8_t  vm_read8(pid_t, void *);
INLINE uint16_t vm_read16(pid_t, void *);
INLINE uint32_t vm_read32(pid_t, void *);
INLINE uint64_t vm_read64(pid_t, void *);
INLINE    float  vm_readf(pid_t, void *);
INLINE   double  vm_readd(pid_t, void *);
INLINE  ssize_t   vm_read(pid_t, void *, void *, ssize_t);
INLINE  ssize_t  vm_read0(pid_t, void *, void *, ssize_t, void *, ssize_t, int);

INLINE     void  vm_write8(pid_t, void *, uint8_t);
INLINE     void vm_write16(pid_t, void *, uint16_t);
INLINE     void vm_write32(pid_t, void *, uint32_t);
INLINE     void vm_write64(pid_t, void *, uint64_t);
INLINE     void  vm_writef(pid_t, void *, float);
INLINE     void  vm_writed(pid_t, void *, double);
INLINE  ssize_t   vm_write(pid_t, void *, void *, ssize_t);


uint8_t vm_read8(pid_t pid, void *targetaddr)
{
    uint8_t ret;
    struct iovec local = {
        &ret,
        1
    }, remote = {
        targetaddr,
        1
    };
    process_vm_readv(pid, &local, 1, &remote, 1, 0);
    return ret;
}

uint16_t vm_read16(pid_t pid, void *targetaddr)
{
    uint16_t ret;
    struct iovec local = {
        &ret,
        2
    }, remote = {
        targetaddr,
        2
    };
    process_vm_readv(pid, &local, 1, &remote, 1, 0);
    return ret;
}

uint32_t vm_read32(pid_t pid, void *targetaddr)
{
    uint32_t ret;
    struct iovec local = {
        &ret,
        4
    }, remote = {
        targetaddr,
        4
    };
    process_vm_readv(pid, &local, 1, &remote, 1, 0);
    return ret;
}

uint64_t vm_read64(pid_t pid, void *targetaddr)
{
    uint64_t ret;
    struct iovec local = {
        &ret,
        8
    }, remote = {
        targetaddr,
        8
    };
    process_vm_readv(pid, &local, 1, &remote, 1, 0);
    return ret;
}

float vm_readf(pid_t pid, void *targetaddr)
{
    float ret;
    struct iovec local = {
        &ret,
        sizeof(float)
    }, remote = {
        targetaddr,
        sizeof(float)
    };
    process_vm_readv(pid, &local, 1, &remote, 1, 0);
    return ret;
} 

double vm_readd(pid_t pid, void *targetaddr)
{
    double ret;
    struct iovec local = {
        &ret,
        sizeof(double)
    }, remote = {
        targetaddr,
        sizeof(double)
    };
    process_vm_readv(pid, &local, 1, &remote, 1, 0);
    return ret;
} 

ssize_t vm_read(pid_t pid, void *targetaddr, void *ret, ssize_t sz)
{
    ssize_t n = 0, m;
    struct iovec remote = {
        targetaddr,
        sz
    }, local = {
        ret,
        sz
    };
    do {
        m = process_vm_readv(pid, &local, 1, &remote, 1, 0);
        if (m < 0)
            return errno < 0 ? errno : -errno;
        n += m;
        remote.iov_base += m;
        local.iov_base += m;
    } while (n < sz);

    return n;
}

ssize_t vm_read0(pid_t pid, void *targetaddr, void *ret, ssize_t sz, void *del, 
                 ssize_t elemsz, int checkmem)
{
    ssize_t n = sz; // = 64 < sz ? 64 : sz;
    ssize_t j, status, red = 0;

    do {
        if ((status = vm_read(pid, targetaddr + red, ret + red, n)) < 0)
            return status;
        for (j=0; j < n; j += elemsz) {
            if (( checkmem && !memcmp(ret + red + j, del, elemsz))
              || !checkmem && ((ptrdiff_t) del) 
                              == MASKEDN(elemsz, *(void **) (ret + red + j)))
                return red + j;

        }
        red += j;
        if (n < MAXSIZE)
            n = INCREASE(n, sz - red);
        if (n > MAXSIZE)
            n = MAXSIZE;
    } while (red <= sz - elemsz);
    return sz + n;
}

void vm_write8(pid_t pid, void *targetaddr, uint8_t value)
{
    struct iovec local = {
        &value,
        1
    }, remote = {
        targetaddr,
        1
    };
    process_vm_writev(pid, &local, 1, &remote, 1, 0);
}

void vm_write16(pid_t pid, void *targetaddr, uint16_t value)
{
    struct iovec local = {
        &value,
        2
    }, remote = {
        targetaddr,
        2
    };
    process_vm_writev(pid, &local, 1, &remote, 1, 0);
}

void vm_write32(pid_t pid, void *targetaddr, uint32_t value)
{
    struct iovec local = {
        &value,
        4
    }, remote = {
        targetaddr,
        4
    };
    process_vm_writev(pid, &local, 1, &remote, 1, 0);
}

void vm_write64(pid_t pid, void *targetaddr, uint64_t value)
{
    struct iovec local = {
        &value,
        8
    }, remote = {
        targetaddr,
        8
    };
    process_vm_writev(pid, &local, 1, &remote, 1, 0);
}

void vm_writef(pid_t pid, void *targetaddr, float value)
{
    struct iovec local = {
        &value,
        sizeof(float)
    }, remote = {
        targetaddr,
        sizeof(float)
    };
    process_vm_writev(pid, &local, 1, &remote, 1, 0);
}

void vm_writed(pid_t pid, void *targetaddr, double value)
{
    struct iovec local = {
        &value,
        sizeof(double)
    }, remote = {
        targetaddr,
        sizeof(double)
    };
    process_vm_writev(pid, &local, 1, &remote, 1, 0);
}


ssize_t vm_write(pid_t pid, void *targetaddr, void *data, ssize_t sz)
{
    ssize_t n = 0, m;
    struct iovec remote = {
        targetaddr,
        sz
    }, local = {
        data,
        sz
    };
    do {
        m = process_vm_writev(pid, &local, 1, &remote, 1, 0);
        if (m < 0)
            return errno < 0 ? errno : -errno;
        n += m;
        remote.iov_base += m;
        local.iov_base += m;
    } while (n < sz);

    return n;
}

