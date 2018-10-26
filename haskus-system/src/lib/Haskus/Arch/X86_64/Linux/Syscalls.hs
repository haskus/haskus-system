{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Linux syscalls on X86_64
module Haskus.Arch.X86_64.Linux.Syscalls where

import Haskus.Arch.X86_64.Linux.Syscall
import Haskus.Format.Binary.Word
import Haskus.Format.String (CString)
import Haskus.Format.Binary.Ptr
import Haskus.Arch.X86_64.Linux.SyscallTable

type FD = Word -- file descriptor alias

-- =============================================================
-- From linux/arch/x86/entry/syscalls/syscall_64.tbl
-- =============================================================

[syscalls|
   -- Linux X86_64 system calls
   0   PrimOp read                    :: FD -> Ptr () -> Word64 -> IO Int64
   1   PrimOp write                   :: FD -> Ptr () -> Word64 -> IO Int64
   2   PrimOp open                    :: CString -> Int -> Word -> IO Int64
   3   PrimOp close                   :: FD -> IO Int64
   4   PrimOp stat                    :: CString -> Ptr () -> IO Int64
   5   PrimOp fstat                   :: FD -> Ptr () -> IO Int64
   6   PrimOp lstat                   :: CString -> Ptr () -> IO Int64
   7   PrimOp poll                    :: Ptr () -> Word64 -> Int64 -> IO Int64
   8   PrimOp lseek                   :: FD -> Int64 -> Int -> IO Int64
   9   PrimOp mmap                    :: Ptr () -> Word64 -> Int64 -> Int64 -> FD -> Word64 -> IO Int64
   10  PrimOp mprotect                :: Ptr () -> Word64 -> Int64 -> IO Int64
   11  PrimOp munmap                  :: Ptr () -> Word64 -> IO Int64
   12  PrimOp brk                     :: Word64 -> IO Int64
   13  PrimOp rt_sigaction            :: Int -> Ptr () -> Ptr () -> IO Int64
   14  PrimOp rt_sigprocmask          :: Int -> Ptr () -> Ptr () -> IO Int64
   15  PrimOp rt_sigreturn            :: IO Int64
   16  PrimOp ioctl                   :: FD -> Int64 -> Int64 -> IO Int64
   17  PrimOp pread64                 :: FD -> Ptr () -> Word64 -> Word64 -> IO Int64
   18  PrimOp pwrite64                :: FD -> Ptr () -> Word64 -> Word64 -> IO Int64
   19  PrimOp readv                   :: FD -> Ptr () -> Int -> IO Int64
   20  PrimOp writev                  :: FD -> Ptr () -> Int -> IO Int64
   21  PrimOp access                  :: CString -> Word64 -> IO Int64
   22  PrimOp pipe                    :: Ptr Word -> IO Int64
   23  Safe   select                  :: Int -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> IO Int64
   24  PrimOp sched_yield             :: IO Int64
   25  PrimOp mremap                  :: Ptr () -> Word64 -> Word64 -> Int -> Ptr () -> IO Int64
   26  PrimOp msync                   :: Ptr () -> Word64 -> Int64 -> IO Int64
   27  PrimOp mincore                 :: Ptr () -> Word64 -> Ptr Word8 -> IO Int64
   28  PrimOp madvise                 :: Ptr () -> Word64 -> Int -> IO Int64
   29  PrimOp shmget                  :: Int32 -> Word64 -> Int -> IO Int64
   30  PrimOp shmat                   :: Int -> Ptr () -> Int -> IO Int64
   31  PrimOp shmctl                  :: Int -> Int -> Ptr () -> IO Int64
   32  PrimOp dup                     :: FD -> IO Int64
   33  PrimOp dup2                    :: FD -> FD -> IO Int64
   34  PrimOp pause                   :: IO Int64
   35  PrimOp nanosleep               :: Ptr () -> Ptr () -> IO Int64
   36  PrimOp getitimer               :: Int -> Ptr () -> IO Int64
   37  PrimOp alarm                   :: Word -> IO Int64
   38  PrimOp setitimer               :: Int -> Ptr () -> Ptr () -> IO Int64
   39  PrimOp getpid                  :: IO Int64
   40  PrimOp sendfile                :: FD -> FD -> Ptr Word64 -> Word64 -> IO Int64
   41  PrimOp socket                  :: Int -> Word64 -> Int -> IO Int64
   42  PrimOp connect                 :: FD -> Ptr () -> Int -> IO Int64
   44  PrimOp sendto                  :: FD -> Ptr () -> Word64 -> Int -> Ptr () -> Word32 -> IO Int64
   45  PrimOp recvfrom                :: FD -> Ptr () -> Word64 -> Word64 -> Ptr () -> Ptr Word64 -> IO Int64
   46  PrimOp sendmsg                 :: FD -> Ptr () -> Int -> IO Int64
   47  PrimOp recvmsg                 :: FD -> Ptr () -> Int -> IO Int64
   48  PrimOp shutdown                :: FD -> Int -> IO Int64
   49  PrimOp bind                    :: FD -> Ptr () -> Int -> IO Int64
   50  PrimOp listen                  :: FD -> Word64 -> IO Int64
   51  PrimOp getsockname             :: FD -> Ptr () -> Ptr Int32 -> IO Int64
   52  PrimOp getpeername             :: FD -> Ptr () -> Ptr Int32 -> IO Int64
   53  PrimOp socketpair              :: Int -> Word64 -> Int -> Ptr Word -> IO Int64
   54  PrimOp setsockopt              :: Int -> Int -> Int -> Ptr () -> Int32 -> IO Int64
   55  PrimOp getsockopt              :: Int -> Int -> Int -> Ptr () -> Ptr Int32 -> IO Int64
   56  Safe   clone                   :: Ptr () -> Ptr () -> Int -> Ptr () -> Ptr () -> Ptr () -> IO Int64
   57  Safe   fork                    :: IO Int64
   58  Safe   vfork                   :: IO Int64
   59  Safe   execve                  :: CString -> Ptr CString -> Ptr CString -> IO Int64
   60  PrimOp exit                    :: Int64 -> IO Int64
   61  Safe   wait4                   :: Word -> Ptr Int -> Int -> Ptr () -> IO Int64
   62  PrimOp kill                    :: Int64 -> Int -> IO Int64
   63  PrimOp uname                   :: Ptr () -> IO Int64
   64  PrimOp semget                  :: Int32 -> Int -> Int -> IO Int64
   65  PrimOp semop                   :: Int -> Ptr () -> Word64 -> IO Int64
   66  PrimOp semctl                  :: Int -> Int -> Int -> Ptr () -> IO Int64
   67  PrimOp shmdt                   :: Ptr () -> IO Int64
   68  PrimOp msgget                  :: Int32 -> Int -> IO Int64
   69  PrimOp msgsnd                  :: Int -> Ptr () -> Word64 -> Int -> IO Int64
   70  PrimOp msgrcv                  :: Int -> Ptr () -> Word64 -> Word64 -> Int -> IO Int64
   71  PrimOp msgctl                  :: Int -> Int -> Ptr () -> IO Int64
   72  PrimOp fcntl                   :: FD -> Int64 -> Int64 -> IO Int64
   73  PrimOp flock                   :: FD -> Int64 -> IO Int64
   74  Safe   fsync                   :: FD -> IO Int64
   75  Safe   fdatasync               :: FD -> IO Int64
   76  PrimOp truncate                :: CString -> Word64 -> IO Int64
   77  PrimOp ftruncate               :: FD -> Word64 -> IO Int64
   78  PrimOp getdents                :: Int -> Ptr () -> Int -> IO Int64
   79  PrimOp getcwd                  :: CString -> Word64 -> IO Int64
   80  PrimOp chdir                   :: CString -> IO Int64
   81  PrimOp fchdir                  :: FD -> IO Int64
   82  PrimOp rename                  :: CString -> CString -> IO Int64
   83  PrimOp mkdir                   :: CString -> Word64 -> IO Int64
   84  PrimOp rmdir                   :: CString -> IO Int64
   85  PrimOp creat                   :: CString -> Word -> IO Int64
   86  PrimOp link                    :: CString -> CString -> IO Int64
   87  PrimOp unlink                  :: CString -> IO Int64
   88  PrimOp symlink                 :: CString -> CString -> IO Int64
   89  PrimOp readlink                :: CString -> CString -> Word64 -> IO Int64
   90  PrimOp chmod                   :: CString -> Word -> IO Int64
   91  PrimOp fchmod                  :: FD -> Word -> IO Int64
   92  PrimOp chown                   :: CString -> Word32 -> Word32 -> IO Int64
   93  PrimOp fchown                  :: FD -> Word32 -> Word32 -> IO Int64
   94  PrimOp lchown                  :: CString -> Word32 -> Word32 -> IO Int64
   95  PrimOp umask                   :: Word -> IO Int64
   96  PrimOp gettimeofday            :: Ptr () -> Ptr () -> IO Int64
   97  PrimOp getrlimit               :: Int -> Ptr () -> IO Int64
   98  PrimOp getrusage               :: Int -> Ptr () -> IO Int64
   99  PrimOp sysinfo                 :: Ptr () -> IO Int64
   100 PrimOp times                   :: Ptr () -> IO Int64
   101 PrimOp ptrace                  :: Int -> Word32 -> Ptr () -> Ptr () -> IO Int64
   102 PrimOp getuid                  :: IO Int64
   103 PrimOp syslog                  :: Int -> CString -> Int -> IO Int64
   104 PrimOp getgid                  :: IO Int64
   105 PrimOp setuid                  :: Word32 -> IO Int64
   106 PrimOp setgid                  :: Word32 -> IO Int64
   107 PrimOp geteuid                 :: IO Int64
   108 PrimOp getegid                 :: IO Int64
   109 PrimOp setpgid                 :: Word32 -> Word32 -> IO Int64
   110 PrimOp getppid                 :: IO Int64
   111 PrimOp getpgrp                 :: Word32 -> IO Int64
   112 PrimOp setsid                  :: IO Int64
   113 PrimOp setreuid                :: Word32 -> Word32 -> IO Int64
   114 PrimOp setregid                :: Word32 -> Word32 -> IO Int64
   115 PrimOp getgroups               :: Int -> Ptr Word32 -> IO Int64
   116 PrimOp setgroups               :: Word64 -> Ptr Word32 -> IO Int64
   117 PrimOp setresuid               :: Word32 -> Word32 -> Word32 -> IO Int64
   118 PrimOp getresuid               :: Ptr Word32 -> Ptr Word32 -> Ptr Word32 -> IO Int64
   119 PrimOp setresgid               :: Word32 -> Word32 -> Word32 -> IO Int64
   120 PrimOp getresgid               :: Ptr Word32 -> Ptr Word32 -> Ptr Word32 -> IO Int64
   121 PrimOp getpgid                 :: Word32 -> IO Int64
   122 PrimOp setfsuid                :: Word32 -> IO Int64
   123 PrimOp setfsgid                :: Word32 -> IO Int64
   124 PrimOp getsid                  :: Word32 -> IO Int64
   125 PrimOp capget                  :: Ptr () -> Ptr () -> IO Int64
   126 PrimOp capset                  :: Ptr () -> Ptr () -> IO Int64
   127 PrimOp rt_sigpending           :: Ptr () -> IO Int64
   128 Safe   rt_sigtimedwait         :: Ptr () -> Ptr () -> Ptr () -> IO Int64
   129 PrimOp rt_sigqueueinfo         :: Word -> Int -> Ptr () -> IO Int64
   130 Safe   rt_sigsuspend           :: Ptr () -> IO Int64
   131 PrimOp rt_sigaltstack          :: Ptr () -> Ptr () -> IO Int64
   132 PrimOp utime                   :: CString -> Ptr () -> IO Int64
   133 PrimOp mknod                   :: CString -> Word64 -> Word64 -> IO Int64
   135 PrimOp personality             :: Word64 -> IO Int64
   136 PrimOp ustat                   :: Word64 -> Ptr () -> IO Int64
   137 PrimOp statfs                  :: CString -> Ptr () -> IO Int64
   138 PrimOp fstatfs                 :: FD -> Ptr () -> IO Int64
   139 PrimOp sysfs                   :: Int -> Word64 -> Ptr () -> IO Int64
   140 PrimOp getpriority             :: Int -> Word64 -> IO Int64
   141 PrimOp setpriority             :: Int -> Word64 -> Int -> IO Int64
   142 PrimOp sched_setparam          :: Word -> Ptr () -> IO Int64
   143 PrimOp sched_getparam          :: Word -> Ptr () -> IO Int64
   144 PrimOp sched_setscheduler      :: Word -> Int -> Ptr () -> IO Int64
   145 PrimOp sched_getscheduler      :: Word -> IO Int64
   146 PrimOp sched_get_priority_max  :: Int -> IO Int64
   147 PrimOp sched_get_priority_min  :: Int -> IO Int64
   148 PrimOp sched_rr_get_interval   :: Word -> Ptr () -> IO Int64
   149 PrimOp mlock                   :: Ptr () -> Word64 -> IO Int64
   150 PrimOp munlock                 :: Ptr () -> Word64 -> IO Int64
   151 PrimOp mlockall                :: Word64 -> IO Int64
   152 PrimOp munlockall              :: IO Int64
   153 PrimOp vhangup                 :: IO Int64
   154 PrimOp modify_ldt              :: Int -> Ptr () -> Word64 -> IO Int64
   155 PrimOp pivot_root              :: CString -> CString -> IO Int64
   156 PrimOp sysctl                  :: Ptr () -> IO Int64
   157 PrimOp prctl                   :: Int -> Word64 -> Word64 -> Word64 -> Word64 -> IO Int64
   158 PrimOp arch_prctl              :: Int -> Word64 -> IO Int64
   159 PrimOp adjtimex                :: Ptr () -> IO Int64
   160 PrimOp setrlimit               :: Int -> Ptr () -> IO Int64
   161 PrimOp chroot                  :: CString -> IO Int64
   162 Safe   sync                    :: IO Int64
   163 PrimOp acct                    :: CString -> IO Int64
   164 PrimOp settimeofday            :: Ptr () -> Ptr () -> IO Int64
   165 PrimOp mount                   :: CString -> CString -> CString -> Word64 -> Ptr () -> IO Int64
   166 PrimOp umount2                 :: CString -> Word64 -> IO Int64
   167 PrimOp swapon                  :: CString -> Int -> IO Int64
   168 PrimOp swapoff                 :: CString -> IO Int64
   169 PrimOp reboot                  :: Word64 -> Word64 -> Word64 -> CString -> IO Int64
   170 PrimOp sethostname             :: CString -> Word64 -> IO Int64
   171 PrimOp setdomainname           :: CString -> Word64 -> IO Int64
   172 PrimOp iopl                    :: Int -> IO Int64
   173 PrimOp ioperm                  :: Word64 -> Word64 -> Int -> IO Int64
   174 PrimOp create_module           :: CString -> Word64 -> IO Int64
   175 PrimOp init_module             :: Ptr () -> Word64 -> CString -> IO Int64
   176 PrimOp delete_module           :: CString -> Int -> IO Int64
   179 PrimOp quotactl                :: Int -> CString -> Int -> CString -> IO Int64
   186 PrimOp gettid                  :: IO Int64
   187 PrimOp readahead               :: Int -> Word64 -> Word64 -> IO Int64
   188 PrimOp setxattr                :: CString -> CString -> Ptr () -> Word64 -> Int -> IO Int64
   189 PrimOp lsetxattr               :: CString -> CString -> Ptr () -> Word64 -> Int -> IO Int64
   190 PrimOp fsetxattr               :: FD -> CString -> Ptr () -> Word64 -> Int -> IO Int64
   191 PrimOp getxattr                :: CString -> CString -> Ptr () -> Word64 -> IO Int64
   192 PrimOp lgetxattr               :: CString -> CString -> Ptr () -> Word64 -> IO Int64
   193 PrimOp fgetxattr               :: FD -> CString -> Ptr () -> Word64 -> IO Int64
   194 PrimOp listxattr               :: CString -> CString -> Word64 -> IO Int64
   195 PrimOp llistxattr              :: CString -> CString -> Word64 -> IO Int64
   196 PrimOp flistxattr              :: FD -> CString -> Word64 -> IO Int64
   197 PrimOp removexattr             :: CString -> CString -> IO Int64
   198 PrimOp lremovexattr            :: CString -> CString -> IO Int64
   199 PrimOp fremovexattr            :: FD -> CString -> IO Int64
   200 PrimOp tkill                   :: Int -> Int -> IO Int64
   201 PrimOp time                    :: Ptr () -> IO Int64
   202 PrimOp futex                   :: Ptr Int64 -> Int -> Int64 -> Ptr () -> Ptr Int64 -> Int64 -> IO Int64
   203 PrimOp sched_setaffinity       :: Word -> Word64 -> Ptr () -> IO Int64
   204 PrimOp sched_getaffinity       :: Word -> Word64 -> Ptr () -> IO Int64
   206 PrimOp io_setup                :: Word64 -> Ptr () -> IO Int64
   207 PrimOp io_destroy              :: Word64 -> IO Int64
   208 PrimOp io_getevents            :: Word64 -> Word64 -> Word64 -> Ptr () -> Ptr () -> IO Int64
   209 PrimOp io_submit               :: Word64 -> Word64 -> Ptr () -> IO Int64
   210 PrimOp io_cancel               :: Word64 -> Ptr () -> Ptr () -> IO Int64
   212 PrimOp lookup_dcookie          :: Word64 -> CString -> Word64 -> IO Int64
   216 PrimOp remap_file_pages        :: Ptr () -> Word64 -> Int -> Word64 -> Int -> IO Int64
   217 PrimOp getdents64              :: FD -> Ptr () -> Word -> IO Int64
   218 PrimOp set_tid_address         :: Ptr Int -> IO Int64
   219 PrimOp restart_syscall         :: IO Int64
   220 PrimOp semtimedop              :: Int -> Ptr () -> Word64 -> Ptr () -> IO Int64
   221 PrimOp fadvise64               :: FD -> Word64 -> Word64 -> Int -> IO Int64
   222 PrimOp timer_create            :: Int32 -> Ptr () -> Ptr () -> IO Int64
   223 PrimOp timer_settime           :: Ptr () -> Int -> Ptr () -> Ptr () -> IO Int64
   224 PrimOp timer_gettime           :: Ptr () -> Ptr () -> IO Int64
   225 PrimOp timer_getoverrun        :: Ptr () -> IO Int64
   226 PrimOp timer_delete            :: Ptr () -> IO Int64
   227 PrimOp clock_settime           :: Int -> Ptr () -> IO Int64
   228 PrimOp clock_gettime           :: Int -> Ptr () -> IO Int64
   229 PrimOp clock_getres            :: Int -> Ptr () -> IO Int64
   230 PrimOp clock_nanosleep         :: Int -> Int -> Ptr () -> Ptr () -> IO Int64
   231 Safe   exit_group              :: Int -> IO Int64
   232 PrimOp epoll_wait              :: Int -> Ptr () -> Int -> Int -> IO Int64
   233 PrimOp epoll_ctl               :: Int -> Int -> Int -> Ptr () -> IO Int64
   234 PrimOp tgkill                  :: Int -> Int -> Int -> IO Int64
   235 PrimOp utimes                  :: CString -> Ptr () -> IO Int64
   237 PrimOp mbind                   :: Ptr () -> Word64 -> Int -> Ptr () -> Word64 -> Word64 -> IO Int64
   238 PrimOp set_mempolicy           :: Int -> Ptr () -> Word64 -> IO Int64
   239 PrimOp get_mempolicy           :: Ptr Int -> Ptr () -> Word64 -> Ptr () -> Word64 -> IO Int64
   240 PrimOp mq_open                 :: CString -> Int -> Word -> Ptr () -> IO Int64
   241 PrimOp mq_unlink               :: CString -> IO Int64
   242 PrimOp mq_timedsend            :: Int -> CString -> Word64 -> Word -> IO Int64
   243 PrimOp mq_timedreceive         :: Int -> CString -> Word64 -> Ptr Word -> IO Int64
   244 PrimOp mq_notify               :: Int -> Ptr () -> IO Int64
   245 PrimOp mq_getsetattr           :: Int -> Ptr () -> Ptr () -> IO Int64
   246 PrimOp kexec_load              :: Word64 -> Word64 -> Ptr () -> Word64 -> IO Int64
   247 PrimOp waitid                  :: Int -> Int -> Ptr () -> Int -> Ptr () -> IO Int64
   248 PrimOp add_key                 :: CString -> CString -> Ptr () -> Word64 -> Int32 -> IO Int64
   249 PrimOp request_key             :: CString -> CString -> Ptr () -> Int32 -> IO Int64
   250 PrimOp keyctl                  :: Int -> Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> IO Int64
   251 PrimOp ioprio_set              :: Int -> Int -> Int -> IO Int64
   252 PrimOp ioprio_get              :: Int -> Int -> IO Int64
   253 PrimOp inotify_init            :: IO Int64
   254 PrimOp inotify_add_watch       :: Int -> CString -> Word32 -> IO Int64
   255 PrimOp inotify_rm_watch        :: Int -> Int -> IO Int64
   256 PrimOp migrate_pages           :: Int -> Word64 -> Ptr Word64 -> Ptr Word64 -> IO Int64
   257 PrimOp openat                  :: FD -> CString -> Int -> Word -> IO Int64
   258 PrimOp mkdirat                 :: FD -> CString -> Word64 -> IO Int64
   259 PrimOp mknodat                 :: FD -> CString -> Word64 -> Word64 -> IO Int64
   260 PrimOp fchownat                :: FD -> CString -> Int -> Int -> Int -> IO Int64
   261 PrimOp futimesat               :: FD -> CString -> Ptr () -> IO Int64
   262 PrimOp fstatat64               :: FD -> CString -> Ptr () -> Int -> IO Int64
   263 PrimOp unlinkat                :: FD -> CString -> Word -> IO Int64
   264 PrimOp renameat                :: FD -> CString -> FD -> CString -> IO Int64
   265 PrimOp linkat                  :: FD -> CString -> FD -> CString -> Int -> IO Int64
   266 PrimOp symlinkat               :: CString -> FD -> CString -> IO Int64
   267 PrimOp readlinkat              :: FD -> CString -> CString -> Word64 -> IO Int64
   268 PrimOp fchmodat                :: FD -> CString -> Word -> Int -> IO Int64
   269 PrimOp faccessat               :: FD -> CString -> Int -> Int -> IO Int64
   270 Safe   pselect                 :: Int -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> Ptr () -> IO Int64
   271 PrimOp ppoll                   :: Ptr () -> Word64 -> Ptr () -> Ptr () -> IO Int64
   272 PrimOp unshare                 :: Int -> IO Int64
   273 PrimOp set_robust_list         :: Ptr () -> Word64 -> IO Int64
   274 PrimOp get_robust_list         :: Int -> Ptr () -> Ptr Word64 -> IO Int64
   275 PrimOp splice                  :: Int -> Ptr () -> Int -> Ptr () -> Word64 -> Word -> IO Int64
   276 PrimOp tee                     :: Int -> Int -> Word64 -> Word -> IO Int64
   277 PrimOp sync_file_range         :: FD -> Word64 -> Word64 -> Word -> IO Int64
   278 PrimOp vmsplice                :: FD -> Ptr () -> Word64 -> Word -> IO Int64
   279 PrimOp move_pages              :: Int -> Word64 -> Ptr () -> Ptr Int -> Int -> Int -> IO Int64
   280 PrimOp utimensat               :: Int -> CString -> Ptr () -> Int -> IO Int64
   281 Safe   epoll_pwait             :: Int -> Ptr () -> Int -> Int -> Ptr () -> IO Int64
   282 PrimOp signalfd                :: FD -> Word64 -> Ptr () -> IO Int64
   283 PrimOp timerfd_create          :: Int -> Int -> IO Int64
   284 PrimOp eventfd                 :: Int -> IO Int64
   285 PrimOp fallocate               :: FD -> Int -> Word64 -> Word64 -> IO Int64
   286 PrimOp timerfd_settime         :: FD -> Int -> Ptr () -> Ptr () -> IO Int64
   287 PrimOp timerfd_gettime         :: FD -> Ptr () -> IO Int64
   288 PrimOp accept4                 :: FD -> Ptr () -> Int -> Word64 -> IO Int64
   289 PrimOp signalfd4               :: FD -> Word64 -> Ptr () -> Int -> IO Int64
   290 PrimOp eventfd2                :: Int -> Int -> IO Int64
   291 PrimOp epoll_create1           :: Word64 -> IO Int64
   292 PrimOp dup3                    :: FD -> FD -> Int -> IO Int64
   293 PrimOp pipe2                   :: Ptr FD -> Int -> IO Int64
   294 PrimOp inotify_init1           :: Int -> IO Int64
   295 PrimOp preadv                  :: FD -> Ptr () -> Int -> Word32 -> Word32 -> IO Int64
   296 PrimOp pwritev                 :: FD -> Ptr () -> Int -> Word32 -> Word32 -> IO Int64
   297 PrimOp rt_tgsigqueueinfo       :: Int -> Int -> Int -> Ptr () -> IO Int64
   298 PrimOp perf_event_open         :: Ptr () -> Int -> Int -> Int -> Word64 -> IO Int64
   299 PrimOp recvmmsg                :: FD -> Ptr () -> Word -> Word -> Ptr () -> IO Int64
   300 PrimOp fanotify_init           :: Int -> Int -> IO Int64
   301 PrimOp fanotify_mark           :: Int -> Word -> Word64 -> Int -> CString -> IO Int64
   302 PrimOp prlimit64               :: Int -> Int -> Ptr () -> Ptr () -> IO Int64
   303 PrimOp name_to_handle_at       :: FD -> CString -> Ptr () -> Ptr Int -> Int -> IO Int64
   304 PrimOp open_to_handle_at       :: Int -> Ptr () -> Int -> IO Int64
   305 PrimOp clock_adjtime           :: Int -> Ptr () -> IO Int64
   306 Safe   syncfs                  :: FD -> IO Int64
   307 PrimOp sendmmsg                :: FD -> Ptr () -> Word -> Word -> IO Int64
   308 PrimOp setns                   :: FD -> Int -> IO Int64
   309 PrimOp getcpu                  :: Ptr Word -> Ptr Word -> Ptr () -> IO Int64
   310 PrimOp process_vm_readv        :: Int -> Ptr () -> Word64 -> Ptr () -> Word64 -> Word64 -> IO Int64
   311 PrimOp process_vm_writev       :: Int -> Ptr () -> Word64 -> Ptr () -> Word64 -> Word64 -> IO Int64
   312 PrimOp kcmp                    :: Int -> Int -> Int -> Word64 -> Word64 -> IO Int64
   313 PrimOp finit_module            :: FD -> CString -> Word -> IO Int64
   314 PrimOp sched_setattr           :: Int -> Ptr () -> Word -> IO Int64
   315 PrimOp sched_getattr           :: Int -> Ptr () -> Word -> Word -> IO Int64
   316 PrimOp renameat2               :: FD -> CString -> FD -> CString -> Word -> IO Int64
   317 PrimOp seccomp                 :: Word -> Word -> Ptr () -> IO Int64
   318 PrimOp getrandom               :: Ptr () -> Word64 -> Word -> IO Int64
   319 PrimOp memfd_create            :: CString -> Word -> IO Int64
   320 PrimOp kexec_file_load         :: FD -> FD -> Word64 -> CString -> Word64 -> IO Int64
   321 PrimOp bpf                     :: Int -> Ptr () -> Word -> IO Int64
   322 PrimOp execveat                :: FD -> CString -> Ptr CString -> Ptr CString -> Int -> IO Int64
   323 PrimOp userfaultfd             :: Int -> IO Int64
   324 PrimOp membarrier              :: Int -> Int -> IO Int64
   325 PrimOp mlock2                  :: Ptr () -> Word64 -> Int -> IO Int64
   326 PrimOp copy_file_range         :: FD -> Ptr Word64 -> FD -> Ptr Word64 -> Word64 -> Word -> IO Int64
   327 PrimOp preadv2                 :: FD -> Ptr () -> Int -> Word64 -> Int -> IO Int64
   328 PrimOp pwritev2                :: FD -> Ptr () -> Int -> Word64 -> Int -> IO Int64
   329 PrimOp pkey_mprotect           :: Word64 -> Word64 -> Word64 -> Int -> IO Int64
   330 PrimOp pkey_alloc              :: Word64 -> Word64 -> IO Int64
   331 PrimOp pkey_free               :: Int -> IO Int64
|]

