    .globl main
main:
    # right operand
    movq	$2, %rax
    push	%rax # save value of right operand on the stack
    # left operand
    movq	$3, %rax
    pop 	%rcx # pop right operand from the stack into %rcx
    subq	%rcx, %rax # subtract right from left (that is in %rax), save result in %rax
    ret
