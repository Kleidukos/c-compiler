    .globl main
main:
    # Function prologue
    push	%ebp # Save old value of %ebp on the stack
    movq	%esp, %ebp # current top of the stack is the bottom of the new stack frame
    # left operand
    movq	$3, %rax
    push	%rax # save value of left operand on the stack
    # right operand
    movq	$2, %rax
    pop 	%rcx # pop left operand from the stack into %rcx
    addq	%rcx, %rax # add left operand to right operand, save result in %rax
    # Function epilogue
    movq	%ebp, %esp # Restore %esp; now it points to the old %ebp
    pop	%ebp # Restore old %ebp; now %esp is where it was before the prologue
    ret
