    .globl main
main:
    # left operand
    movq	$10, %rax
    push	%rax
    # right operand
    movq	$2, %rax
    pop 	%rcx # pop left from the stack into %rcx, right is already in %eax
    cmpq	%rax, %rcx # set ZF on, if left == right, set if off otherwise
    movq	$0, %rax # zero out %rax
    setl	%al # set %al (lower byte of %rax) to 1 iff ZF is on
    ret
