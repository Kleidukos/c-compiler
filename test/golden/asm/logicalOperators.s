    .globl main
main:
    # Function prologue
    push	%rbp # Save old value of %rbp on the stack
    movq	%rsp, %rbp # current top of the stack is the bottom of the new stack frame
    # left operand
    movq	$0, %rax
    cmpq	$0, %rax
    jne	.Lcg0
    jmp	.Lcg1 # jump to end label
.Lcg0:
    # right operand
    movq	$1, %rax
    cmpq	$0, %rax # check if right expr is true
    movq	$0, %rax # zero out %rax
    setne	%al # set %al (low byte of %rax) to 1 iff right expr is true
.Lcg1:	 # end label
    # Function epilogue
    movq	%rbp, %rsp # Restore %rsp; now it points to the old %rbp
    pop	%rbp # Restore old %rbp; now %rsp is where it was before the prologue
    ret
