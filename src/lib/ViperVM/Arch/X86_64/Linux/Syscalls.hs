module ViperVM.Arch.X86_64.Linux.Syscalls
   ( syscall_access
   , syscall_chdir
   , syscall_chmod
   , syscall_chown
   , syscall_close
   , syscall_creat
   , syscall_dup
   , syscall_dup2
   , syscall_fchdir
   , syscall_fchmod
   , syscall_fchown
   , syscall_fdatasync
   , syscall_flock
   , syscall_epoll_create1
   , syscall_fstat
   , syscall_mount
   , syscall_umount2
   , syscall_poll
   , syscall_read
   , syscall_pread64
   , syscall_readv
   , syscall_preadv
   , syscall_write
   , syscall_pwrite64
   , syscall_writev
   , syscall_pwritev
   , syscall_futex
   , syscall_uname
   , syscall_mmap
   , syscall_munmap
   , syscall_mprotect
   , syscall_brk
   , syscall_recvfrom
   , syscall_shutdown
   , syscall_sendfile
   , syscall_socket
   , syscall_socketpair
   , syscall_bind
   , syscall_connect
   , syscall_accept4
   , syscall_madvise
   , syscall_listen
   , syscall_pipe
   , syscall_msync
   , syscall_reboot
   , syscall_exit
   , syscall_getcpu
   , syscall_sched_yield
   , syscall_getpid
   , syscall_getppid
   , syscall_fork
   , syscall_vfork
   , syscall_getegid
   , syscall_gettid
   , syscall_pause
   , syscall_alarm
   , syscall_clock_gettime
   , syscall_clock_settime
   , syscall_clock_getres
   , syscall_nanosleep
   , syscall_ptrace
   , syscall_sigprocmask
   , syscall_kill
   , syscall_getuid
   , syscall_geteuid
   , syscall_getgid
   , syscall_setgid
   , syscall_setuid
   , syscall_mlock
   , syscall_munlock
   , syscall_mlockall
   , syscall_munlockall
   , syscall_mincore
   , syscall_mkdir
   , syscall_mkdirat
   , syscall_fsync
   , syscall_ftruncate
   , syscall_getcwd
   , syscall_ioctl
   , syscall_lchown
   , syscall_link
   , syscall_lseek
   , syscall_lstat
   , syscall_mknod
   , syscall_mknodat
   , syscall_getdents64
   , syscall_rmdir
   , syscall_open
   , syscall_openat
   , syscall_rename
   , syscall_stat
   , syscall_symlink
   , syscall_sync
   , syscall_syncfs
   , syscall_truncate
   , syscall_umask
   , syscall_unlink
   , syscall_unlinkat
   )
   where

import ViperVM.Arch.X86_64.Linux.Syscall

import Foreign.C.String (CString)
import Foreign.Ptr
import Data.Word
import Data.Int

type FD = Word -- file descriptor alias
type Mode = Word
type AccessMode = Word64
type SizeT = Word

-- | read
syscall_read :: FD -> Ptr a -> Word64 -> IO Int64
syscall_read = syscall3 0
{-# INLINE syscall_read #-}

-- | write
syscall_write :: FD -> Ptr a -> Word64 -> IO Int64
syscall_write = syscall3 1
{-# INLINE syscall_write #-}

-- | open
syscall_open :: CString -> Int -> Mode -> IO Int64
syscall_open = syscall3 2
{-# INLINE syscall_open #-}

-- | close
syscall_close :: FD -> IO Int64
syscall_close = syscall1 3
{-# INLINE syscall_close #-}

-- | stat
syscall_stat :: CString -> Ptr a -> IO Int64
syscall_stat = syscall2 4
{-# INLINE syscall_stat #-}

-- | fstat
syscall_fstat :: FD -> Ptr a -> IO Int64
syscall_fstat = syscall2 5
{-# INLINE syscall_fstat #-}

-- | lstat
syscall_lstat :: CString -> Ptr a -> IO Int64
syscall_lstat = syscall2 6
{-# INLINE syscall_lstat #-}

-- | poll
syscall_poll :: Ptr a -> Word64 -> Int64 -> IO Int64
syscall_poll = syscall3 7
{-# INLINE syscall_poll #-}

-- | lseek
syscall_lseek :: FD -> Int64 -> Int -> IO Int64
syscall_lseek = syscall3 8
{-# INLINE syscall_lseek #-}

-- | mmap
syscall_mmap :: Ptr a -> Word64 -> Int64 -> Int64 -> FD -> Word64 -> IO Int64
syscall_mmap = syscall6 9
{-# INLINE syscall_mmap #-}

-- | mprotect
syscall_mprotect :: Ptr a -> Word64 -> Int64 -> IO Int64
syscall_mprotect = syscall3 10
{-# INLINE syscall_mprotect #-}

-- | munmap
syscall_munmap :: Ptr a -> Word64 -> IO Int64
syscall_munmap = syscall2 11
{-# INLINE syscall_munmap #-}

-- | brk
syscall_brk :: Word64 -> IO Int64
syscall_brk = syscall1 12
{-# INLINE syscall_brk #-}

-- | sigprocmask
syscall_sigprocmask :: Int -> Ptr a -> Ptr b -> IO Int64
syscall_sigprocmask = syscall3 14
{-# INLINE syscall_sigprocmask #-}

-- | ioctl
syscall_ioctl :: FD -> Int64 -> Int64 -> IO Int64
syscall_ioctl = syscall3 16
{-# INLINE syscall_ioctl #-}

-- | pread64
syscall_pread64 :: FD -> Ptr a -> Word64 -> Word64 -> IO Int64
syscall_pread64 = syscall4 17
{-# INLINE syscall_pread64 #-}

-- | pwrite64
syscall_pwrite64 :: FD -> Ptr a -> Word64 -> Word64 -> IO Int64
syscall_pwrite64 = syscall4 18
{-# INLINE syscall_pwrite64 #-}

-- | readv
syscall_readv :: FD -> Ptr a -> Int -> IO Int64
syscall_readv = syscall3 19
{-# INLINE syscall_readv #-}

-- | writev
syscall_writev :: FD -> Ptr a -> Int-> IO Int64
syscall_writev = syscall3 20
{-# INLINE syscall_writev #-}

-- | access
syscall_access :: CString -> AccessMode -> IO Int64
syscall_access = syscall2 21
{-# INLINE syscall_access #-}

-- | pipe
syscall_pipe :: Ptr Word -> IO Int64
syscall_pipe = syscall1 22
{-# INLINE syscall_pipe #-}

-- | sched_yield
syscall_sched_yield :: IO Int64
syscall_sched_yield = syscall0 24
{-# INLINE syscall_sched_yield #-}

-- | msync
syscall_msync :: Ptr a -> Word64 -> Int64 -> IO Int64
syscall_msync = syscall3 26
{-# INLINE syscall_msync #-}

-- | mincore
syscall_mincore :: Ptr a -> Word64 -> Ptr Word8 -> IO Int64
syscall_mincore = syscall3 27
{-# INLINE syscall_mincore #-}

-- | madvise
syscall_madvise :: Ptr a -> Word64 -> Int -> IO Int64
syscall_madvise = syscall3 28
{-# INLINE syscall_madvise #-}

-- | dup
syscall_dup :: FD -> IO Int64
syscall_dup = syscall1 32
{-# INLINE syscall_dup #-}

-- | dup2
syscall_dup2 :: FD -> FD -> IO Int64
syscall_dup2 = syscall2 33
{-# INLINE syscall_dup2 #-}

-- | pause
syscall_pause :: IO Int64
syscall_pause = syscall0 34
{-# INLINE syscall_pause #-}

-- | nanosleep
syscall_nanosleep :: Ptr a -> Ptr a -> IO Int64
syscall_nanosleep = syscall2 35
{-# INLINE syscall_nanosleep #-}

-- | alarm
syscall_alarm :: Word -> IO Int64
syscall_alarm = syscall1 37
{-# INLINE syscall_alarm #-}

-- | getpid
syscall_getpid :: IO Int64
syscall_getpid = syscall0 39
{-# INLINE syscall_getpid #-}

-- | sendfile
syscall_sendfile :: FD -> FD -> Ptr Word64 -> Word64 -> IO Int64
syscall_sendfile = syscall4 40
{-# INLINE syscall_sendfile #-}

-- | socket
syscall_socket :: Int -> Word64 -> Int -> IO Int64
syscall_socket = syscall3 41
{-# INLINE syscall_socket #-}

-- | connect
syscall_connect :: FD -> Ptr a -> Int -> IO Int64
syscall_connect = syscall3 42
{-# INLINE syscall_connect #-}

-- | bind
syscall_bind :: FD -> Ptr a -> Int -> IO Int64
syscall_bind = syscall3 49
{-# INLINE syscall_bind #-}

-- | listen
syscall_listen :: FD -> Word64 -> IO Int64
syscall_listen = syscall2 50
{-# INLINE syscall_listen #-}

-- | socketpair
syscall_socketpair :: Int -> Word64 -> Int -> Ptr Word -> IO Int64
syscall_socketpair = syscall4 53
{-# INLINE syscall_socketpair #-}

-- | recvfrom
syscall_recvfrom :: FD -> Ptr () -> Word64 -> Word64 -> Ptr a -> Ptr Word64 -> IO Int64
syscall_recvfrom = syscall6 45
{-# INLINE syscall_recvfrom #-}

-- | shutdown
syscall_shutdown :: FD -> Int -> IO Int64
syscall_shutdown = syscall2 48
{-# INLINE syscall_shutdown #-}

-- | fork
syscall_fork :: IO Int64
syscall_fork = syscall0 57
{-# INLINE syscall_fork #-}

-- | vfork
syscall_vfork :: IO Int64
syscall_vfork = syscall0 58
{-# INLINE syscall_vfork #-}

-- | exit
syscall_exit :: Int64 -> IO Int64
syscall_exit = syscall1 60
{-# INLINE syscall_exit #-}

-- | kill
syscall_kill :: Int64 -> Int -> IO Int64
syscall_kill = syscall2 62
{-# INLINE syscall_kill #-}

-- | uname
syscall_uname :: CString -> IO Int64
syscall_uname = syscall1 63
{-# INLINE syscall_uname #-}

-- | flock
syscall_flock :: FD -> Int64 -> IO Int64
syscall_flock = syscall2 73
{-# INLINE syscall_flock #-}

-- | fsync
syscall_fsync :: FD -> IO Int64
syscall_fsync = syscall1 74
{-# INLINE syscall_fsync #-}

-- | fdatasync
syscall_fdatasync :: FD -> IO Int64
syscall_fdatasync = syscall1 75
{-# INLINE syscall_fdatasync #-}

-- | truncate
syscall_truncate :: CString -> Word64 -> IO Int64
syscall_truncate = syscall2 76
{-# INLINE syscall_truncate #-}

-- | ftruncate
syscall_ftruncate :: FD -> Word64 -> IO Int64
syscall_ftruncate = syscall2 77
{-# INLINE syscall_ftruncate #-}

-- | getcwd
syscall_getcwd :: CString -> SizeT -> IO Int64
syscall_getcwd = syscall2 79
{-# INLINE syscall_getcwd #-}

-- | chdir
syscall_chdir :: CString -> IO Int64
syscall_chdir = syscall1 80
{-# INLINE syscall_chdir #-}

-- | fchdir
syscall_fchdir :: FD -> IO Int64
syscall_fchdir = syscall1 81
{-# INLINE syscall_fchdir #-}

-- | rename
syscall_rename :: CString -> CString -> IO Int64
syscall_rename = syscall2 82
{-# INLINE syscall_rename #-}

-- | mkdir
syscall_mkdir :: CString -> Word64 -> IO Int64
syscall_mkdir = syscall2 83
{-# INLINE syscall_mkdir #-}

-- | rmdir
syscall_rmdir :: CString -> IO Int64
syscall_rmdir = syscall1 84
{-# INLINE syscall_rmdir #-}

-- | creat
syscall_creat :: CString -> Mode -> IO Int64
syscall_creat = syscall2 85
{-# INLINE syscall_creat #-}

-- | link
syscall_link :: CString -> CString -> IO Int64
syscall_link = syscall2 86
{-# INLINE syscall_link #-}

-- | unlink
syscall_unlink :: CString -> IO Int64
syscall_unlink = syscall1 87
{-# INLINE syscall_unlink #-}

-- | symlink
syscall_symlink :: CString -> CString -> IO Int64
syscall_symlink = syscall2 88
{-# INLINE syscall_symlink #-}

-- | chmod
syscall_chmod :: CString -> Mode -> IO Int64
syscall_chmod = syscall2 90
{-# INLINE syscall_chmod #-}

-- | fchmod
syscall_fchmod :: FD -> Mode -> IO Int64
syscall_fchmod = syscall2 91
{-# INLINE syscall_fchmod #-}

-- | chown
syscall_chown :: CString -> Word32 -> Word32 -> IO Int64
syscall_chown = syscall3 92
{-# INLINE syscall_chown #-}

-- | fchown
syscall_fchown :: FD -> Word32 -> Word32 -> IO Int64
syscall_fchown = syscall3 93
{-# INLINE syscall_fchown #-}

-- | lchown
syscall_lchown :: CString -> Word32 -> Word32 -> IO Int64
syscall_lchown = syscall3 94
{-# INLINE syscall_lchown #-}

-- | umask
syscall_umask :: Word -> IO Int64
syscall_umask = syscall1 95
{-# INLINE syscall_umask #-}

-- | ptrace
syscall_ptrace :: Int -> Word32 -> Ptr () -> Ptr () -> IO Int64
syscall_ptrace = syscall4 101
{-# INLINE syscall_ptrace #-}

-- | getuid
syscall_getuid :: IO Int64
syscall_getuid = syscall0 102
{-# INLINE syscall_getuid #-}

-- | getgid
syscall_getgid :: IO Int64
syscall_getgid = syscall0 104
{-# INLINE syscall_getgid #-}

-- | setuid
syscall_setuid :: Word32 -> IO Int64
syscall_setuid = syscall1 105
{-# INLINE syscall_setuid #-}

-- | setgid
syscall_setgid :: Word32 -> IO Int64
syscall_setgid = syscall1 106
{-# INLINE syscall_setgid #-}

-- | geteuid
syscall_geteuid :: IO Int64
syscall_geteuid = syscall0 107
{-# INLINE syscall_geteuid #-}

-- | getegid
syscall_getegid :: IO Int64
syscall_getegid = syscall0 108
{-# INLINE syscall_getegid #-}

-- | getppid
syscall_getppid :: IO Int64
syscall_getppid = syscall0 110
{-# INLINE syscall_getppid #-}

-- | mknod
syscall_mknod :: CString -> Word64 -> Ptr a -> IO Int64
syscall_mknod = syscall3 133
{-# INLINE syscall_mknod #-}

-- | mlock
syscall_mlock :: Ptr a -> Word64 -> IO Int64
syscall_mlock = syscall2 149
{-# INLINE syscall_mlock #-}

-- | munlock
syscall_munlock :: Ptr a -> Word64 -> IO Int64
syscall_munlock = syscall2 150
{-# INLINE syscall_munlock #-}

-- | mlockall
syscall_mlockall :: Word64 -> IO Int64
syscall_mlockall = syscall1 151
{-# INLINE syscall_mlockall #-}

-- | munlockall
syscall_munlockall :: IO Int64
syscall_munlockall = syscall0 152
{-# INLINE syscall_munlockall #-}

-- | sync
syscall_sync :: IO Int64
syscall_sync = syscall0 162
{-# INLINE syscall_sync #-}

-- | mount
syscall_mount :: CString -> CString -> CString -> Word64 -> Ptr a -> IO Int64
syscall_mount = syscall5 165
{-# INLINE syscall_mount #-}

-- | umount2
syscall_umount2 :: CString -> Word64 -> IO Int64
syscall_umount2 = syscall2 166
{-# INLINE syscall_umount2 #-}

-- | reboot
syscall_reboot :: Word64 -> Word64 -> Word64 -> CString -> IO Int64
syscall_reboot = syscall4 169
{-# INLINE syscall_reboot #-}

-- | gettid
syscall_gettid :: IO Int64
syscall_gettid = syscall0 186
{-# INLINE syscall_gettid #-}

-- | futex
syscall_futex :: Ptr Int64 -> Int -> Int64 -> Ptr a -> Ptr Int64 -> Int64 -> IO Int64
syscall_futex = syscall6 202
{-# INLINE syscall_futex #-}

-- | getdents64
syscall_getdents64 :: FD -> Ptr a -> Word -> IO Int64
syscall_getdents64 = syscall3 217
{-# INLINE syscall_getdents64 #-}

-- | clock_settime
syscall_clock_settime :: Int -> Ptr a -> IO Int64
syscall_clock_settime = syscall2 227
{-# INLINE syscall_clock_settime #-}

-- | clock_gettime
syscall_clock_gettime :: Int -> Ptr a -> IO Int64
syscall_clock_gettime = syscall2 228
{-# INLINE syscall_clock_gettime #-}

-- | clock_getres
syscall_clock_getres :: Int -> Ptr a -> IO Int64
syscall_clock_getres = syscall2 229
{-# INLINE syscall_clock_getres #-}

-- | openat
syscall_openat :: FD -> CString -> Int -> Mode -> IO Int64
syscall_openat = syscall4 257
{-# INLINE syscall_openat #-}

-- | mkdirat
syscall_mkdirat :: FD -> CString -> Word64 -> IO Int64
syscall_mkdirat = syscall3 258
{-# INLINE syscall_mkdirat #-}

-- | mknodat
syscall_mknodat :: FD -> CString -> Word64 -> Ptr a -> IO Int64
syscall_mknodat = syscall4 259
{-# INLINE syscall_mknodat #-}

-- | unlinkat
syscall_unlinkat :: FD -> CString -> Word -> IO Int64
syscall_unlinkat = syscall3 263
{-# INLINE syscall_unlinkat #-}

-- | accept4
syscall_accept4 :: FD -> Ptr a -> Int -> Word64 -> IO Int64
syscall_accept4 = syscall4 288
{-# INLINE syscall_accept4 #-}

-- | epoll_create1
syscall_epoll_create1 :: Word64 -> IO Int64
syscall_epoll_create1 = syscall1 291
{-# INLINE syscall_epoll_create1 #-}

-- | preadv
syscall_preadv :: FD -> Ptr a -> Int -> Word32 -> Word32 -> IO Int64
syscall_preadv = syscall5 295
{-# INLINE syscall_preadv #-}

-- | pwritev
syscall_pwritev :: FD -> Ptr a -> Int -> Word32 -> Word32 -> IO Int64
syscall_pwritev = syscall5 296
{-# INLINE syscall_pwritev #-}

-- | syncfs
syscall_syncfs :: FD -> IO Int64
syscall_syncfs = syscall1 306
{-# INLINE syscall_syncfs #-}

-- | getcpu
syscall_getcpu :: Ptr Word -> Ptr Word -> Ptr a -> IO Int64
syscall_getcpu = syscall3 309
{-# INLINE syscall_getcpu #-}


{-
 - Remaining syscalls to wrap
 -
13	64	rt_sigaction		sys_rt_sigaction
15	64	rt_sigreturn		stub_rt_sigreturn
23	common	select			sys_select
25	common	mremap			sys_mremap
29	common	shmget			sys_shmget
30	common	shmat			sys_shmat
31	common	shmctl			sys_shmctl
36	common	getitimer		sys_getitimer
38	common	setitimer		sys_setitimer
44	common	sendto			sys_sendto
46	64	sendmsg			sys_sendmsg
47	64	recvmsg			sys_recvmsg
51	common	getsockname		sys_getsockname
52	common	getpeername		sys_getpeername
54	64	setsockopt		sys_setsockopt
55	64	getsockopt		sys_getsockopt
56	common	clone			stub_clone
59	64	execve			stub_execve
61	common	wait4			sys_wait4
64	common	semget			sys_semget
65	common	semop			sys_semop
66	common	semctl			sys_semctl
67	common	shmdt			sys_shmdt
68	common	msgget			sys_msgget
69	common	msgsnd			sys_msgsnd
70	common	msgrcv			sys_msgrcv
71	common	msgctl			sys_msgctl
72	common	fcntl			sys_fcntl
78	common	getdents		sys_getdents
89	common	readlink		sys_readlink
96	common	gettimeofday		sys_gettimeofday
97	common	getrlimit		sys_getrlimit
98	common	getrusage		sys_getrusage
99	common	sysinfo			sys_sysinfo
100	common	times			sys_times
103	common	syslog			sys_syslog
109	common	setpgid			sys_setpgid
111	common	getpgrp			sys_getpgrp
112	common	setsid			sys_setsid
113	common	setreuid		sys_setreuid
114	common	setregid		sys_setregid
115	common	getgroups		sys_getgroups
116	common	setgroups		sys_setgroups
117	common	setresuid		sys_setresuid
118	common	getresuid		sys_getresuid
119	common	setresgid		sys_setresgid
120	common	getresgid		sys_getresgid
121	common	getpgid			sys_getpgid
122	common	setfsuid		sys_setfsuid
123	common	setfsgid		sys_setfsgid
124	common	getsid			sys_getsid
125	common	capget			sys_capget
126	common	capset			sys_capset
127	64	rt_sigpending		sys_rt_sigpending
128	64	rt_sigtimedwait		sys_rt_sigtimedwait
129	64	rt_sigqueueinfo		sys_rt_sigqueueinfo
130	common	rt_sigsuspend		sys_rt_sigsuspend
131	64	sigaltstack		sys_sigaltstack
132	common	utime			sys_utime
134	64	uselib
135	common	personality		sys_personality
136	common	ustat			sys_ustat
137	common	statfs			sys_statfs
138	common	fstatfs			sys_fstatfs
139	common	sysfs			sys_sysfs
140	common	getpriority		sys_getpriority
141	common	setpriority		sys_setpriority
142	common	sched_setparam		sys_sched_setparam
143	common	sched_getparam		sys_sched_getparam
144	common	sched_setscheduler	sys_sched_setscheduler
145	common	sched_getscheduler	sys_sched_getscheduler
146	common	sched_get_priority_max	sys_sched_get_priority_max
147	common	sched_get_priority_min	sys_sched_get_priority_min
148	common	sched_rr_get_interval	sys_sched_rr_get_interval
153	common	vhangup			sys_vhangup
154	common	modify_ldt		sys_modify_ldt
155	common	pivot_root		sys_pivot_root
156	64	_sysctl			sys_sysctl
157	common	prctl			sys_prctl
158	common	arch_prctl		sys_arch_prctl
159	common	adjtimex		sys_adjtimex
160	common	setrlimit		sys_setrlimit
161	common	chroot			sys_chroot
163	common	acct			sys_acct
164	common	settimeofday		sys_settimeofday
167	common	swapon			sys_swapon
168	common	swapoff			sys_swapoff
170	common	sethostname		sys_sethostname
171	common	setdomainname		sys_setdomainname
172	common	iopl			stub_iopl
173	common	ioperm			sys_ioperm
174	64	create_module
175	common	init_module		sys_init_module
176	common	delete_module		sys_delete_module
177	64	get_kernel_syms
178	64	query_module
179	common	quotactl		sys_quotactl
180	64	nfsservctl
181	common	getpmsg
182	common	putpmsg
183	common	afs_syscall
184	common	tuxcall
185	common	security
187	common	readahead		sys_readahead
188	common	setxattr		sys_setxattr
189	common	lsetxattr		sys_lsetxattr
190	common	fsetxattr		sys_fsetxattr
191	common	getxattr		sys_getxattr
192	common	lgetxattr		sys_lgetxattr
193	common	fgetxattr		sys_fgetxattr
194	common	listxattr		sys_listxattr
195	common	llistxattr		sys_llistxattr
196	common	flistxattr		sys_flistxattr
197	common	removexattr		sys_removexattr
198	common	lremovexattr		sys_lremovexattr
199	common	fremovexattr		sys_fremovexattr
200	common	tkill			sys_tkill
201	common	time			sys_time
203	common	sched_setaffinity	sys_sched_setaffinity
204	common	sched_getaffinity	sys_sched_getaffinity
205	64	set_thread_area
206	common	io_setup		sys_io_setup
207	common	io_destroy		sys_io_destroy
208	common	io_getevents		sys_io_getevents
209	common	io_submit		sys_io_submit
210	common	io_cancel		sys_io_cancel
211	64	get_thread_area
212	common	lookup_dcookie		sys_lookup_dcookie
214	64	epoll_ctl_old
215	64	epoll_wait_old
216	common	remap_file_pages	sys_remap_file_pages
218	common	set_tid_address		sys_set_tid_address
219	common	restart_syscall		sys_restart_syscall
220	common	semtimedop		sys_semtimedop
221	common	fadvise64		sys_fadvise64
222	64	timer_create		sys_timer_create
223	common	timer_settime		sys_timer_settime
224	common	timer_gettime		sys_timer_gettime
225	common	timer_getoverrun	sys_timer_getoverrun
226	common	timer_delete		sys_timer_delete
230	common	clock_nanosleep		sys_clock_nanosleep
231	common	exit_group		sys_exit_group
232	common	epoll_wait		sys_epoll_wait
233	common	epoll_ctl		sys_epoll_ctl
234	common	tgkill			sys_tgkill
235	common	utimes			sys_utimes
236	64	vserver
237	common	mbind			sys_mbind
238	common	set_mempolicy		sys_set_mempolicy
239	common	get_mempolicy		sys_get_mempolicy
240	common	mq_open			sys_mq_open
241	common	mq_unlink		sys_mq_unlink
242	common	mq_timedsend		sys_mq_timedsend
243	common	mq_timedreceive		sys_mq_timedreceive
244	64	mq_notify		sys_mq_notify
245	common	mq_getsetattr		sys_mq_getsetattr
246	64	kexec_load		sys_kexec_load
247	64	waitid			sys_waitid
248	common	add_key			sys_add_key
249	common	request_key		sys_request_key
250	common	keyctl			sys_keyctl
251	common	ioprio_set		sys_ioprio_set
252	common	ioprio_get		sys_ioprio_get
253	common	inotify_init		sys_inotify_init
254	common	inotify_add_watch	sys_inotify_add_watch
255	common	inotify_rm_watch	sys_inotify_rm_watch
256	common	migrate_pages		sys_migrate_pages
260	common	fchownat		sys_fchownat
261	common	futimesat		sys_futimesat
262	common	newfstatat		sys_newfstatat
264	common	renameat		sys_renameat
265	common	linkat			sys_linkat
266	common	symlinkat		sys_symlinkat
267	common	readlinkat		sys_readlinkat
268	common	fchmodat		sys_fchmodat
269	common	faccessat		sys_faccessat
270	common	pselect6		sys_pselect6
271	common	ppoll			sys_ppoll
272	common	unshare			sys_unshare
273	64	set_robust_list		sys_set_robust_list
274	64	get_robust_list		sys_get_robust_list
275	common	splice			sys_splice
276	common	tee			sys_tee
277	common	sync_file_range		sys_sync_file_range
278	64	vmsplice		sys_vmsplice
279	64	move_pages		sys_move_pages
280	common	utimensat		sys_utimensat
281	common	epoll_pwait		sys_epoll_pwait
282	common	signalfd		sys_signalfd
283	common	timerfd_create		sys_timerfd_create
284	common	eventfd			sys_eventfd
285	common	fallocate		sys_fallocate
286	common	timerfd_settime		sys_timerfd_settime
287	common	timerfd_gettime		sys_timerfd_gettime
289	common	signalfd4		sys_signalfd4
290	common	eventfd2		sys_eventfd2
292	common	dup3			sys_dup3
293	common	pipe2			sys_pipe2
294	common	inotify_init1		sys_inotify_init1
297	64	rt_tgsigqueueinfo	sys_rt_tgsigqueueinfo
298	common	perf_event_open		sys_perf_event_open
299	64	recvmmsg		sys_recvmmsg
300	common	fanotify_init		sys_fanotify_init
301	common	fanotify_mark		sys_fanotify_mark
302	common	prlimit64		sys_prlimit64
303	common	name_to_handle_at	sys_name_to_handle_at
304	common	open_by_handle_at	sys_open_by_handle_at
305	common	clock_adjtime		sys_clock_adjtime
307	64	sendmmsg		sys_sendmmsg
308	common	setns			sys_setns
310	64	process_vm_readv	sys_process_vm_readv
311	64	process_vm_writev	sys_process_vm_writev
312	common	kcmp			sys_kcmp
313	common	finit_module		sys_finit_module
314	common	sched_setattr		sys_sched_setattr
315	common	sched_getattr		sys_sched_getattr
316   renameat2
317   seccomp
318   getrandom
319   memfd_create
320   kexec_file_load
321   bpf
-}
