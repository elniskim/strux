.text
.balign 16
.globl main
main:
	endbr64
	pushq %rbp
	movq %rsp, %rbp
	subq $48, %rsp
	movq $0, -40(%rbp)
	movq $1, -32(%rbp)
	movq $2, -24(%rbp)
	movq $3, -16(%rbp)
	movq $4, -8(%rbp)
	movq -40(%rbp), %rcx
	subq $32, %rsp
	callq printInt
	subq $-32, %rsp
	movq -32(%rbp), %rcx
	subq $32, %rsp
	callq printInt
	subq $-32, %rsp
	movq -24(%rbp), %rcx
	subq $32, %rsp
	callq printInt
	subq $-32, %rsp
	movq -16(%rbp), %rcx
	subq $32, %rsp
	callq printInt
	subq $-32, %rsp
	movq -8(%rbp), %rcx
	subq $32, %rsp
	callq printInt
	subq $-32, %rsp
	leave
	ret
/* end function main */

