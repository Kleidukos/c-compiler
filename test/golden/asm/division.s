    .globl main
main:
    # Function prologue
    push	%ebp # Save old value of %ebp on the stack
    movq	%esp, %ebp # current top of the stack is the bottom of the new stack frame
    # right operand
    movq	$2, %rax
    push	%rax # save value of right operand on the stack
    # left operand
    movq	$10, %rax
    pop 	%rcx # pop right operand from the stack into %rcx
    cdq
    idivq	%rcx # divide left by right (that is in %rax), save result in %rax
    # Function epilogue
    movq	%ebp, %esp # Restore %esp; now it points to the old %ebp
    pop	%ebp # Restore old %ebp; now %esp is where it was before the prologue
    ret
