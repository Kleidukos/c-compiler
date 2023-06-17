    .globl main
main:
    # left operand
    movq	$3, %rax
    push	%rax # save value of left operand on the stack
    # right operand
    movq	$2, %rax
    pop	%rcx # pop left operand from the stack into %rcx
    addq	%rcx, %rax # add left operand to right operand, save result in %rax
    ret
