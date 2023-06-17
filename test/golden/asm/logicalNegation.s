    .globl main
main:
    movq	$1, %rax
    cmpq	$0, %rax
    movq	$0, %rax
    sete	%al
    ret
