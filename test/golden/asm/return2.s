    .globl main
main:
    # Function prologue
    push	%ebp # Save old value of %ebp on the stack
    movq	%esp, %ebp # current top of the stack is the bottom of the new stack frame
    movq	$2, %rax
    # Function epilogue
    movq	%ebp, %esp # Restore %esp; now it points to the old %ebp
    pop	%ebp # Restore old %ebp; now %esp is where it was before the prologue
    ret
