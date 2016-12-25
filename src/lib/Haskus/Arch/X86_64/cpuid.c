/**********************************************************
 * Convertion between GHC STG calling convention on x86-64
 * and CPUID instruction
 * ********************************************************
 *
 * STG:
 *    Virtual registers: Base, Sp, Hp,  R1-R6,                      SpLim
 *    Registers:         r13, rbp, r12, rbx, r14, rsi, rdi, r8, r9, r15
 *    Use tail-call (jump) to Sp[0] to exit a function, so the
 *    "return value" is to be stored in rbx (and following if we
 *    want to return a tuple)
 *
 * CPUID:
 *    Parameter EAX
 *    Results: EAX, EBX, EDX, ECX
 *
 */
void x86_64_cpuid_primop() {
   asm (
      "movq %%rbx, %%rax\n\t"
      "cpuid\n\t"
      "movq %%rbx, %%r14\n\t"
      "movq %%rax, %%rbx\n\t"
      "movq %%rdx, %%rdi\n\t"
      "movq %%rcx, %%rsi\n\t"
      "jmp * (%%rbp)\n\t"
      ::: 
   );
}

/* CPUID with EAX and ECX as parameters */
void x86_64_cpuid2_primop() {
   asm (
      "movq %%rbx, %%rax\n\t"
      "movq %%r14, %%rcx\n\t"
      "cpuid\n\t"
      "movq %%rbx, %%r14\n\t"
      "movq %%rax, %%rbx\n\t"
      "movq %%rdx, %%rdi\n\t"
      "movq %%rcx, %%rsi\n\t"
      "jmp * (%%rbp)\n\t"
      ::: 
   );
}

/*************************************************
 * Convertion between x86-64 calling convention
 * and CPUID instruction
 * ***********************************************
 *
 * x86-64: 
 *    Parameters: rdi, rsi, rdx, rcx, r8, r9
 *    Values returned: rax, rdx
 *    Caller-save: rax, rdi, rsi, rdx, rcx, r8, r9, r10, r11
 *    Callee-save: rbx, rsp, rbp, r12, r13, r14, r15 
 *
 * CPUID:
 *    Parameter EAX
 *    Results: EAX, EBX, EDX, ECX
 */
void x86_64_cpuid_ffi() {
   asm (
      "movq %%rdi, %%rax\n\t"
      "movq %%rbx, %%r8\n\t" //save RBX
      "cpuid\n\t"
      "mov %%eax, (%%rsi)\n\t"
      "mov %%ebx, 0x04(%%rsi)\n\t"
      "mov %%edx, 0x0c(%%rsi)\n\t"
      "mov %%ecx, 0x08(%%rsi)\n\t"
      "movq %%r8, %%rbx\n\t" // restore RBX
      :::
   );
}

void x86_64_cpuid2_ffi() {
   asm (
      "movq %%rdi, %%rax\n\t"
      "movq %%rbx, %%r8\n\t" //save RBX
      "movq %%rcx, %%r9\n\t" //save RCX
      "movq %%rsi, %%rcx\n\t"
      "cpuid\n\t"
      "mov %%eax, (%%rsi)\n\t"
      "mov %%ebx, 0x04(%%rsi)\n\t"
      "mov %%edx, 0x0c(%%rsi)\n\t"
      "mov %%ecx, 0x08(%%rsi)\n\t"
      "movq %%r8, %%rbx\n\t" // restore RBX
      "movq %%r9, %%rcx\n\t" // restore RBX
      :::
   );
}
