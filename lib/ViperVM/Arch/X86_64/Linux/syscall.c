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

void x86_64_linux_syscall6() {
   asm (
      "movq %%rdi, %%rax\n\t"
      "movq %%rsi, %%rdi\n\t"
      "movq %%rdx, %%rsi\n\t"
      "movq %%rcx, %%rdx\n\t"
      "movq %%r8,  %%r10\n\t"
      "movq %%r9,  %%r8\n\t"
      "movq 8(%%rsp),%%r9\n\t"
      "syscall\n\t"
      :::
   );
}

void x86_64_linux_syscall5() {
   asm (
      "movq %%rdi, %%rax\n\t"
      "movq %%rsi, %%rdi\n\t"
      "movq %%rdx, %%rsi\n\t"
      "movq %%rcx, %%rdx\n\t"
      "movq %%r8,  %%r10\n\t"
      "movq %%r9,  %%r8\n\t"
      "syscall\n\t"
      :::
   );
}

void x86_64_linux_syscall4() {
   asm (
      "movq %%rdi, %%rax\n\t"
      "movq %%rsi, %%rdi\n\t"
      "movq %%rdx, %%rsi\n\t"
      "movq %%rcx, %%rdx\n\t"
      "movq %%r8,  %%r10\n\t"
      "syscall\n\t"
      :::
   );
}

void x86_64_linux_syscall3() {
   asm (
      "movq %%rdi, %%rax\n\t"
      "movq %%rsi, %%rdi\n\t"
      "movq %%rdx, %%rsi\n\t"
      "movq %%rcx, %%rdx\n\t"
      "syscall\n\t"
      :::
   );
}

void x86_64_linux_syscall2() {
   asm (
      "movq %%rdi, %%rax\n\t"
      "movq %%rsi, %%rdi\n\t"
      "movq %%rdx, %%rsi\n\t"
      "syscall\n\t"
      :::
   );
}

void x86_64_linux_syscall1() {
   asm (
      "movq %%rdi, %%rax\n\t"
      "movq %%rsi, %%rdi\n\t"
      "syscall\n\t"
      :::
   );
}

void x86_64_linux_syscall0() {
   asm (
      "movq %%rdi, %%rax\n\t"
      "syscall\n\t"
      :::
   );
}

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

void x86_64_linux_syscall_primop6() {
   asm (
      "movq %%rbx, %%rax\n\t"
      "movq %%rdi, %%rdx\n\t"
      "movq %%r14, %%rdi\n\t"
      "movq %%r8,  %%r10\n\t"
      "movq %%r9,  %%r8\n\t"
      "movq 8(%%rbp),%%r9\n\t"
      "syscall\n\t"
      "movq %%rax, %%rbx\n\t"
      "jmp * (%%rbp)\n\t"
      ::: 
   );
}

void x86_64_linux_syscall_primop5() {
   asm (
      "movq %%rbx, %%rax\n\t"
      "movq %%rdi, %%rdx\n\t"
      "movq %%r14, %%rdi\n\t"
      "movq %%r8,  %%r10\n\t"
      "movq %%r9,  %%r8\n\t"
      "syscall\n\t"
      "movq %%rax, %%rbx\n\t"
      "jmp * (%%rbp)\n\t"
      ::: 
   );
}

void x86_64_linux_syscall_primop4() {
   asm (
      "movq %%rbx, %%rax\n\t"
      "movq %%rdi, %%rdx\n\t"
      "movq %%r14, %%rdi\n\t"
      "movq %%r8,  %%r10\n\t"
      "syscall\n\t"
      "movq %%rax, %%rbx\n\t"
      "jmp * (%%rbp)\n\t"
      ::: 
   );
}

void x86_64_linux_syscall_primop3() {
   asm (
      "movq %%rbx, %%rax\n\t"
      "movq %%rdi, %%rdx\n\t"
      "movq %%r14, %%rdi\n\t"
      "syscall\n\t"
      "movq %%rax, %%rbx\n\t"
      "jmp * (%%rbp)\n\t"
      ::: 
   );
}

void x86_64_linux_syscall_primop2() {
   asm (
      "movq %%rbx, %%rax\n\t"
      "movq %%r14, %%rdi\n\t"
      "syscall\n\t"
      "movq %%rax, %%rbx\n\t"
      "jmp * (%%rbp)\n\t"
      ::: 
   );
}

void x86_64_linux_syscall_primop1() {
   asm (
      "movq %%rbx, %%rax\n\t"
      "movq %%r14, %%rdi\n\t"
      "syscall\n\t"
      "movq %%rax, %%rbx\n\t"
      "jmp * (%%rbp)\n\t"
      ::: 
   );
}

void x86_64_linux_syscall_primop0() {
   asm (
      "movq %%rbx, %%rax\n\t"
      "syscall\n\t"
      "movq %%rax, %%rbx\n\t"
      "jmp * (%%rbp)\n\t"
      ::: 
   );
}
