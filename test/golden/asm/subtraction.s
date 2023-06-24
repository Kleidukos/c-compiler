    .globl main
main:
    # Function prologue
    push	%rbp # Save old value of %rbp on the stack
    movq	%rsp, %rbp # current top of the stack is the bottom of the new stack frame
    # right operand
    movq	$2, %rax
    push	%rax # save value of right operand on the stack
    # left operand
    movq	$3, %rax
    pop 	%rcx # pop right operand from the stack into %rcx
    subq	%rcx, %rax # subtract right from left (that is in %rax), save result in %rax
    # Function epilogue
    movq	%rbp, %rsp # Restore %rsp; now it points to the old %rbp
    pop	%rbp # Restore old %rbp; now %rsp is where it was before the prologue
    ret
