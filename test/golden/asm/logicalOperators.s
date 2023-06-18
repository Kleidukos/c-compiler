    .globl main
main:
    # left operand
    movq	$0, %rax
    cmpq	$0, %rax
    jne	.L0
    jmp	.L1 # jump to end label
.L0:
    # right operand
    movq	$1, %rax
    cmpq	$0, %rax # check if right expr is true
    movq	$0, %rax # zero out %rax
    setne	%al # set %al (low byte of %rax) to 1 iff right expr is true
.L1:	 # end label
    ret
