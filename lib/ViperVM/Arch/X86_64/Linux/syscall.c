#include <stdint.h>

int64_t x86_64_linux_syscall6(int64_t n, int64_t arg1, int64_t arg2, int64_t arg3, int64_t arg4, int64_t arg5, int64_t arg6) {

   int64_t ret;

   asm volatile (
      "movq %%rdi, %%rax\n\t"
      "movq %%rsi, %%rdi\n\t"
      "movq %%rdx, %%rsi\n\t"
      "movq %%rcx, %%rdx\n\t"
      "movq %%r8,  %%r10\n\t"
      "movq %%r9,  %%r8\n\t"
      "movq 8(%%rsp),%%r9\n\t"
      "syscall\n\t"
      : "=a" (ret)
      :
      : "rcx", "r11", "memory"
   );

   return ret;
}

int64_t x86_64_linux_syscall5(int64_t n, int64_t arg1, int64_t arg2, int64_t arg3, int64_t arg4, int64_t arg5) {

   int64_t ret;

   asm volatile (
      "movq %%rdi, %%rax\n\t"
      "movq %%rsi, %%rdi\n\t"
      "movq %%rdx, %%rsi\n\t"
      "movq %%rcx, %%rdx\n\t"
      "movq %%r8,  %%r10\n\t"
      "movq %%r9,  %%r8\n\t"
      "syscall\n\t"
      : "=a" (ret)
      :
      : "rcx", "r11", "memory"
   );

   return ret;
}

int64_t x86_64_linux_syscall4(int64_t n, int64_t arg1, int64_t arg2, int64_t arg3, int64_t arg4) {

   int64_t ret;

   asm volatile (
      "movq %%rdi, %%rax\n\t"
      "movq %%rsi, %%rdi\n\t"
      "movq %%rdx, %%rsi\n\t"
      "movq %%rcx, %%rdx\n\t"
      "movq %%r8,  %%r10\n\t"
      "syscall\n\t"
      : "=a" (ret)
      :
      : "rcx", "r11", "memory"
   );

   return ret;
}

int64_t x86_64_linux_syscall3(int64_t n, int64_t arg1, int64_t arg2, int64_t arg3) {

   int64_t ret;

   asm volatile (
      "movq %%rdi, %%rax\n\t"
      "movq %%rsi, %%rdi\n\t"
      "movq %%rdx, %%rsi\n\t"
      "movq %%rcx, %%rdx\n\t"
      "syscall\n\t"
      : "=a" (ret)
      :
      : "rcx", "r11", "memory"
   );

   return ret;
}

int64_t x86_64_linux_syscall2(int64_t n, int64_t arg1, int64_t arg2) {

   int64_t ret;

   asm volatile (
      "movq %%rdi, %%rax\n\t"
      "movq %%rsi, %%rdi\n\t"
      "movq %%rdx, %%rsi\n\t"
      "syscall\n\t"
      : "=a" (ret)
      :
      : "rcx", "r11", "memory"
   );

   return ret;
}

int64_t x86_64_linux_syscall1(int64_t n, int64_t arg1) {

   int64_t ret;

   asm volatile (
      "movq %%rdi, %%rax\n\t"
      "movq %%rsi, %%rdi\n\t"
      "syscall\n\t"
      : "=a" (ret)
      :
      : "rcx", "r11", "memory"
   );

   return ret;
}

int64_t x86_64_linux_syscall0(int64_t n) {

   int64_t ret;

   asm volatile (
      "movq %%rdi, %%rax\n\t"
      "syscall\n\t"
      : "=a" (ret)
      :
      : "rcx", "r11", "memory"
   );

   return ret;
}

void x86_64_linux_syscall_stg6() {

   asm volatile (
      "movq %%rbx, %%rax\n\t"
      "movq %%rdi, %%rdx\n\t"
      "movq %%r14, %%rdi\n\t"
      "movq %%r8,  %%r10\n\t"
      "movq %%r9,  %%r8\n\t"
      "movq 8(%%rbp),%%r9\n\t"
      "syscall\n\t"
      "movq %%rax, %%rbx\n\t"
      "addq 8, %%rbp\n\t"
      "jmp * -8(%%rbp)\n\t"
      ::: 
   );

}
