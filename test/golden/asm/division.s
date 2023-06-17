    .globl main
main:
    # right operand
    movq	$2, %rax
    push	%rax # save value of right operand on the stack
    # left operand
    movq	$10, %rax
    pop 	%rcx # pop right operand from the stack into %rcx
    cdq
    idivq	%rcx # divide left by right (that is in %rax), save result in %rax
    ret
