/*
** Landlock confinement for bash tools (todos/bash-tools-v2.md, section 1.4).
**
** agents-exe runs a sandboxed tool through itself as a launcher
** (System.Agents.Tools.Landlock): the launcher calls agents_landlock_exec,
** which builds a ruleset from the given paths, restricts the calling thread
** and execs the tool, all from one C function so the restriction and the
** exec happen on the same OS thread whatever the Haskell runtime does. A
** Landlock domain survives execve and is inherited by every child.
**
** The kernel interface is declared here rather than taken from
** <linux/landlock.h>, so the build does not depend on how recent the
** headers are; what the running kernel supports is asked at run time
** (agents_landlock_abi).
*/
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <sys/prctl.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <unistd.h>

#ifndef __NR_landlock_create_ruleset
#define __NR_landlock_create_ruleset 444
#endif
#ifndef __NR_landlock_add_rule
#define __NR_landlock_add_rule 445
#endif
#ifndef __NR_landlock_restrict_self
#define __NR_landlock_restrict_self 446
#endif

#define LL_CREATE_RULESET_VERSION (1U << 0)
#define LL_RULE_PATH_BENEATH 1

#define LL_FS_EXECUTE (1ULL << 0)
#define LL_FS_WRITE_FILE (1ULL << 1)
#define LL_FS_READ_FILE (1ULL << 2)
#define LL_FS_READ_DIR (1ULL << 3)
#define LL_FS_REFER (1ULL << 13)
#define LL_FS_TRUNCATE (1ULL << 14)
#define LL_FS_IOCTL_DEV (1ULL << 15)

#define LL_SCOPE_SIGNAL (1ULL << 1)

/* Field order is the kernel's; the size passed says how much of it is read. */
struct ll_ruleset_attr {
    uint64_t handled_access_fs;
    uint64_t handled_access_net;
    uint64_t scoped;
};

struct ll_path_beneath_attr {
    uint64_t allowed_access;
    int32_t parent_fd;
} __attribute__((packed));

/* Bits of agents_landlock_exec's modes[]. */
#define MODE_WRITE 1    /* every handled access, not only read and execute */
#define MODE_OPTIONAL 2 /* skip the path when it does not exist */

/*
** The Landlock ABI version of the running kernel (1 and up), or -errno:
** -ENOSYS when the kernel was built without it, -EOPNOTSUPP when it is
** built in but not enabled (missing from the lsm= boot parameter).
*/
int agents_landlock_abi(void) {
    long r = syscall(__NR_landlock_create_ruleset, NULL, 0, LL_CREATE_RULESET_VERSION);
    return r < 0 ? -errno : (int)r;
}

static uint64_t handled_fs(int abi) {
    uint64_t fs = (1ULL << 13) - 1; /* ABI 1: EXECUTE .. MAKE_SYM */
    if (abi >= 2) fs |= LL_FS_REFER;
    if (abi >= 3) fs |= LL_FS_TRUNCATE;
    if (abi >= 5) fs |= LL_FS_IOCTL_DEV;
    return fs;
}

/*
** Confines the calling thread to the n paths (read and execute, or every
** handled access with MODE_WRITE) and execs file with argv in the current
** environment. Only returns on failure, with a message in err.
*/
int agents_landlock_exec(int n, const char **paths, const int *modes, const char *file, char *const argv[], char *err, size_t errlen) {
    int abi = agents_landlock_abi();
    if (abi < 1) {
        snprintf(err, errlen, "Landlock is not available on this kernel: %s", strerror(-abi));
        return -1;
    }
    uint64_t fs = handled_fs(abi);
    uint64_t readx = LL_FS_EXECUTE | LL_FS_READ_FILE | LL_FS_READ_DIR;
    uint64_t file_rights = LL_FS_EXECUTE | LL_FS_WRITE_FILE | LL_FS_READ_FILE | LL_FS_TRUNCATE | LL_FS_IOCTL_DEV;

    struct ll_ruleset_attr attr = {.handled_access_fs = fs, .handled_access_net = 0, .scoped = 0};
    size_t attr_size = sizeof(uint64_t);
    if (abi >= 6) {
        /* Signals only reach processes inside the sandbox, like bwrap's
           --unshare-pid; abstract unix sockets are left alone (net: host). */
        attr.scoped = LL_SCOPE_SIGNAL;
        attr_size = sizeof(attr);
    }
    int ruleset = (int)syscall(__NR_landlock_create_ruleset, &attr, attr_size, 0);
    if (ruleset < 0) {
        snprintf(err, errlen, "landlock_create_ruleset: %s", strerror(errno));
        return -1;
    }
    for (int i = 0; i < n; i++) {
        int fd = open(paths[i], O_PATH | O_CLOEXEC);
        if (fd < 0) {
            if ((modes[i] & MODE_OPTIONAL) && errno == ENOENT) continue;
            snprintf(err, errlen, "cannot open %s for the sandbox: %s", paths[i], strerror(errno));
            close(ruleset);
            return -1;
        }
        struct stat st;
        if (fstat(fd, &st) < 0) {
            snprintf(err, errlen, "cannot stat %s: %s", paths[i], strerror(errno));
            close(fd);
            close(ruleset);
            return -1;
        }
        uint64_t access = (modes[i] & MODE_WRITE) ? fs : readx;
        if (!S_ISDIR(st.st_mode)) access &= file_rights; /* directory rights are refused on a file */
        struct ll_path_beneath_attr rule = {.allowed_access = access & fs, .parent_fd = fd};
        long r = syscall(__NR_landlock_add_rule, ruleset, LL_RULE_PATH_BENEATH, &rule, 0);
        int saved = errno;
        close(fd);
        if (r < 0) {
            snprintf(err, errlen, "landlock_add_rule %s: %s", paths[i], strerror(saved));
            close(ruleset);
            return -1;
        }
    }
    if (prctl(PR_SET_NO_NEW_PRIVS, 1, 0, 0, 0) < 0) {
        snprintf(err, errlen, "prctl(PR_SET_NO_NEW_PRIVS): %s", strerror(errno));
        close(ruleset);
        return -1;
    }
    if (syscall(__NR_landlock_restrict_self, ruleset, 0) < 0) {
        snprintf(err, errlen, "landlock_restrict_self: %s", strerror(errno));
        close(ruleset);
        return -1;
    }
    close(ruleset);
    execvp(file, argv);
    snprintf(err, errlen, "cannot exec %s: %s", file, strerror(errno));
    return -1;
}
