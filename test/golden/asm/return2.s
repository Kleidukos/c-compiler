    .globl main
main:
    # Function prologue
    push	%rbp # Save old value of %rbp on the stack
    movq	%rsp, %rbp # current top of the stack is the bottom of the new stack frame
    movq	$2, %rax
    # Function epilogue
    movq	%rbp, %rsp # Restore %rsp; now it points to the old %rbp
    pop	%rbp # Restore old %rbp; now %rsp is where it was before the prologue
    ret
