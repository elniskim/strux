.text
.balign 16
.globl add5
add5:
	endbr64
	movq %rcx, %rax
	addq $5, %rax
	ret
/* end function add5 */

.text
.balign 16
.globl main
main:
	endbr64
	pushq %rbp
	movq %rsp, %rbp
	subq $32, %rsp
	movl $5, %ecx
	callq add5
	movq %rax, %rcx
	subq $-32, %rsp
	subq $32, %rsp
	callq printInt
	subq $-32, %rsp
	leave
	ret
/* end function main */

