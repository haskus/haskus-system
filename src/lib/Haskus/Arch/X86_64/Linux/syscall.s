/*************************************************
 * Convertion between x86-64 calling convention
 * and Linux (64 bits) system call convention
 * ***********************************************
 *
 * x86-64: 
 *    Parameters: rdi, rsi, rdx, rcx, r8, r9
 *    Values returned: rax, rdx
 *    Caller-save: rax, rdi, rsi, rdx, rcx, r8, r9, r10, r11
 *    Callee-save: rbx, rsp, rbp, r12, r13, r14, r15 
 *
 * Linux:
 *    Parameters: rax (syscall number), rdi, rsi, rdx, r10, r8, r9
 *    Value returned: rax
 *    Caller-save: rcx, r11
 */

.global x86_64_linux_syscall6
x86_64_linux_syscall6:
   movq %rdi, %rax
   movq %rsi, %rdi
   movq %rdx, %rsi
   movq %rcx, %rdx
   movq %r8,  %r10
   movq %r9,  %r8
   movq 8(%rsp),%r9
   syscall
   retq

.global x86_64_linux_syscall5
x86_64_linux_syscall5:
   movq %rdi, %rax
   movq %rsi, %rdi
   movq %rdx, %rsi
   movq %rcx, %rdx
   movq %r8,  %r10
   movq %r9,  %r8
   syscall
   retq

.global x86_64_linux_syscall4
x86_64_linux_syscall4:
   movq %rdi, %rax
   movq %rsi, %rdi
   movq %rdx, %rsi
   movq %rcx, %rdx
   movq %r8,  %r10
   syscall
   retq

.global x86_64_linux_syscall3
x86_64_linux_syscall3:
   movq %rdi, %rax
   movq %rsi, %rdi
   movq %rdx, %rsi
   movq %rcx, %rdx
   syscall
   retq

.global x86_64_linux_syscall2
x86_64_linux_syscall2:
   movq %rdi, %rax
   movq %rsi, %rdi
   movq %rdx, %rsi
   syscall
   retq

.global x86_64_linux_syscall1
x86_64_linux_syscall1:
   movq %rdi, %rax
   movq %rsi, %rdi
   syscall
   retq

.global x86_64_linux_syscall0
x86_64_linux_syscall0:
   movq %rdi, %rax
   syscall
   retq

/**********************************************************
 * Convertion between GHC STG calling convention on x86-64
 * and Linux (64 bits) system call convention
 * ********************************************************
 *
 * STG:
 *    Virtual registers: Base, Sp, Hp, R1-R6, SpLim
 *    Registers: r13, rbp, r12, rbx, r14, rsi, rdi, r8, r9, r15
 *    Use tail-call (jump) to Sp[0] to exit a function, so the
 *    "return value" is to be stored in rbx (and following if we
 *    want to return a tuple)
 *
 * Linux:
 *    Parameters: rax (syscall number), rdi, rsi, rdx, r10, r8, r9
 *    Value returned: rax
 *    Caller-save: rcx, r11
 */

.global x86_64_linux_syscall_primop6
x86_64_linux_syscall_primop6:
   movq %rbx, %rax
   movq %rdi, %rdx
   movq %r14, %rdi
   movq %r8,  %r10
   movq %r9,  %r8
   movq (%rbp), %r9
   add  $0x08, %rbp
   syscall
   movq %rax, %rbx
   jmp * (%rbp)

.global x86_64_linux_syscall_primop5
x86_64_linux_syscall_primop5:
   movq %rbx, %rax
   movq %rdi, %rdx
   movq %r14, %rdi
   movq %r8,  %r10
   movq %r9,  %r8
   syscall
   movq %rax, %rbx
   jmp * (%rbp)

.global x86_64_linux_syscall_primop4
x86_64_linux_syscall_primop4:
   movq %rbx, %rax
   movq %rdi, %rdx
   movq %r14, %rdi
   movq %r8,  %r10
   syscall
   movq %rax, %rbx
   jmp * (%rbp)

.global x86_64_linux_syscall_primop3
x86_64_linux_syscall_primop3:
   movq %rbx, %rax
   movq %rdi, %rdx
   movq %r14, %rdi
   syscall
   movq %rax, %rbx
   jmp * (%rbp)

.global x86_64_linux_syscall_primop2
x86_64_linux_syscall_primop2:
   movq %rbx, %rax
   movq %r14, %rdi
   syscall
   movq %rax, %rbx
   jmp * (%rbp)

.global x86_64_linux_syscall_primop1
x86_64_linux_syscall_primop1:
   movq %rbx, %rax
   movq %r14, %rdi
   syscall
   movq %rax, %rbx
   jmp * (%rbp)

.global x86_64_linux_syscall_primop0
x86_64_linux_syscall_primop0:
   movq %rbx, %rax
   syscall
   movq %rax, %rbx
   jmp * (%rbp)
