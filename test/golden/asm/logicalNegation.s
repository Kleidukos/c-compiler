    .globl main
main:
    movl	$1, %eax
    cmpl	$0, %eax
    movl	$0, %eax
    sete	%al
    ret
