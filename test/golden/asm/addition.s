    .globl main
main:
    # Function prologue
    push	%rbp # Save old value of %rbp on the stack
    movq	%rsp, %rbp # current top of the stack is the bottom of the new stack frame
    # left operand
    movq	$3, %rax
    push	%rax # save value of left operand on the stack
    # right operand
    movq	$2, %rax
    pop 	%rcx # pop left operand from the stack into %rcx
    addq	%rcx, %rax # add left operand to right operand, save result in %rax
    # Function epilogue
    movq	%rbp, %rsp # Restore %rsp; now it points to the old %rbp
    pop	%rbp # Restore old %rbp; now %rsp is where it was before the prologue
    ret
