    .globl main
main:
    # Function prologue
    push	%ebp # Save old value of %ebp on the stack
    movq	%esp, %ebp # current top of the stack is the bottom of the new stack frame
    # left operand
    movq	$10, %rax
    push	%rax
    # right operand
    movq	$2, %rax
    pop 	%rcx # pop left from the stack into %rcx, right is already in %eax
    cmpq	%rax, %rcx # set ZF on, if left == right, set if off otherwise
    movq	$0, %rax # zero out %rax
    setl	%al # set %al (lower byte of %rax) to 1 iff ZF is on
    # Function epilogue
    movq	%ebp, %esp # Restore %esp; now it points to the old %ebp
    pop	%ebp # Restore old %ebp; now %esp is where it was before the prologue
    ret
