IO:
	.string "%lld"
	.text
	.globl _main
_main:
	pushq %rbp
	movq %rsp, %rbp
	subq $32, %rsp
	pushq $10
	movq %rbp, %rax
	leaq -16(%rax), %rax
	popq (%rax)
	movq $80, %rdi
	callq _malloc
	pushq %rax
	movq %rbp, %rax
	leaq -8(%rax), %rax
	popq (%rax)
	pushq $0
	pushq %rbp
	callq init
	addq $16, %rsp
	.data
L10:	.string "before sorting\n"
	.text
	leaq L10(%rip), %rdi
	movq $0, %rax
	callq _printf
	pushq $0
	pushq %rbp
	callq print
	addq $16, %rsp
	pushq $0
	pushq %rbp
	callq sort
	addq $16, %rsp
	.data
L11:	.string "after sorting\n"
	.text
	leaq L11(%rip), %rdi
	movq $0, %rax
	callq _printf
	pushq $0
	pushq %rbp
	callq print
	addq $16, %rsp
	leaveq
	retq
init:
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
	pushq $0
	movq %rbp, %rax
	leaq -8(%rax), %rax
	popq (%rax)
L2:
	movq %rbp, %rax
	leaq -8(%rax), %rax
	movq (%rax), %rax
	pushq %rax
	movq %rbp, %rax
	movq 16(%rax), %rax
	leaq -16(%rax), %rax
	movq (%rax), %rax
	pushq %rax
	popq %rax
	popq %rbx
	cmpq %rax, %rbx
	jge L1
	subq $16, %rsp
	movq %rbp, %rax
	movq 16(%rax), %rax
	leaq -16(%rax), %rax
	movq (%rax), %rax
	pushq %rax
	movq %rbp, %rax
	leaq -8(%rax), %rax
	movq (%rax), %rax
	pushq %rax
	popq %rax
	subq %rax, (%rsp)
	pushq $8
	movq %rbp, %rax
	leaq -8(%rax), %rax
	movq (%rax), %rax
	pushq %rax
	popq %rax
	imulq (%rsp), %rax
	movq %rax, (%rsp)
	movq %rbp, %rax
	movq 16(%rax), %rax
	leaq -8(%rax), %rax
	movq (%rax), %rax
	popq %rbx
	leaq (%rax,%rbx), %rax
	popq (%rax)
	movq %rbp, %rax
	leaq -8(%rax), %rax
	movq (%rax), %rax
	pushq %rax
	pushq $1
	popq %rax
	addq %rax, (%rsp)
	movq %rbp, %rax
	leaq -8(%rax), %rax
	popq (%rax)
	jmp L2
L1:
	leaveq
	retq
print:
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
	pushq $0
	movq %rbp, %rax
	leaq -8(%rax), %rax
	popq (%rax)
L4:
	movq %rbp, %rax
	leaq -8(%rax), %rax
	movq (%rax), %rax
	pushq %rax
	movq %rbp, %rax
	movq 16(%rax), %rax
	leaq -16(%rax), %rax
	movq (%rax), %rax
	pushq %rax
	popq %rax
	popq %rbx
	cmpq %rax, %rbx
	jge L3
	subq $16, %rsp
	pushq $8
	movq %rbp, %rax
	leaq -8(%rax), %rax
	movq (%rax), %rax
	pushq %rax
	popq %rax
	imulq (%rsp), %rax
	movq %rax, (%rsp)
	movq %rbp, %rax
	movq 16(%rax), %rax
	leaq -8(%rax), %rax
	movq (%rax), %rax
	popq %rbx
	leaq (%rax,%rbx), %rax
	movq (%rax), %rax
	pushq %rax
	popq  %rsi
	leaq IO(%rip), %rdi
	movq $0, %rax
	callq _printf
	.data
L5:	.string " "
	.text
	leaq L5(%rip), %rdi
	movq $0, %rax
	callq _printf
	movq %rbp, %rax
	leaq -8(%rax), %rax
	movq (%rax), %rax
	pushq %rax
	pushq $1
	popq %rax
	addq %rax, (%rsp)
	movq %rbp, %rax
	leaq -8(%rax), %rax
	popq (%rax)
	jmp L4
L3:
	.data
L6:	.string "\n"
	.text
	leaq L6(%rip), %rdi
	movq $0, %rax
	callq _printf
	leaveq
	retq
swap:
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
	pushq $8
	movq %rbp, %rax
	leaq 24(%rax), %rax
	movq (%rax), %rax
	pushq %rax
	popq %rax
	imulq (%rsp), %rax
	movq %rax, (%rsp)
	movq %rbp, %rax
	movq 16(%rax), %rax
	movq 16(%rax), %rax
	movq 16(%rax), %rax
	leaq -8(%rax), %rax
	movq (%rax), %rax
	popq %rbx
	leaq (%rax,%rbx), %rax
	movq (%rax), %rax
	pushq %rax
	movq %rbp, %rax
	leaq -8(%rax), %rax
	popq (%rax)
	pushq $8
	movq %rbp, %rax
	leaq 32(%rax), %rax
	movq (%rax), %rax
	pushq %rax
	popq %rax
	imulq (%rsp), %rax
	movq %rax, (%rsp)
	movq %rbp, %rax
	movq 16(%rax), %rax
	movq 16(%rax), %rax
	movq 16(%rax), %rax
	leaq -8(%rax), %rax
	movq (%rax), %rax
	popq %rbx
	leaq (%rax,%rbx), %rax
	movq (%rax), %rax
	pushq %rax
	pushq $8
	movq %rbp, %rax
	leaq 24(%rax), %rax
	movq (%rax), %rax
	pushq %rax
	popq %rax
	imulq (%rsp), %rax
	movq %rax, (%rsp)
	movq %rbp, %rax
	movq 16(%rax), %rax
	movq 16(%rax), %rax
	movq 16(%rax), %rax
	leaq -8(%rax), %rax
	movq (%rax), %rax
	popq %rbx
	leaq (%rax,%rbx), %rax
	popq (%rax)
	movq %rbp, %rax
	leaq -8(%rax), %rax
	movq (%rax), %rax
	pushq %rax
	pushq $8
	movq %rbp, %rax
	leaq 32(%rax), %rax
	movq (%rax), %rax
	pushq %rax
	popq %rax
	imulq (%rsp), %rax
	movq %rax, (%rsp)
	movq %rbp, %rax
	movq 16(%rax), %rax
	movq 16(%rax), %rax
	movq 16(%rax), %rax
	leaq -8(%rax), %rax
	movq (%rax), %rax
	popq %rbx
	leaq (%rax,%rbx), %rax
	popq (%rax)
	leaveq
	retq
min:
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
	movq %rbp, %rax
	leaq 24(%rax), %rax
	movq (%rax), %rax
	pushq %rax
	movq %rbp, %rax
	movq 16(%rax), %rax
	movq 16(%rax), %rax
	leaq -16(%rax), %rax
	movq (%rax), %rax
	pushq %rax
	popq %rax
	popq %rbx
	cmpq %rax, %rbx
	jge L7
	subq $16, %rsp
	pushq $8
	movq %rbp, %rax
	leaq 24(%rax), %rax
	movq (%rax), %rax
	pushq %rax
	popq %rax
	imulq (%rsp), %rax
	movq %rax, (%rsp)
	movq %rbp, %rax
	movq 16(%rax), %rax
	movq 16(%rax), %rax
	leaq -8(%rax), %rax
	movq (%rax), %rax
	popq %rbx
	leaq (%rax,%rbx), %rax
	movq (%rax), %rax
	pushq %rax
	pushq $8
	movq %rbp, %rax
	movq 16(%rax), %rax
	leaq 24(%rax), %rax
	movq (%rax), %rax
	pushq %rax
	popq %rax
	imulq (%rsp), %rax
	movq %rax, (%rsp)
	movq %rbp, %rax
	movq 16(%rax), %rax
	movq 16(%rax), %rax
	leaq -8(%rax), %rax
	movq (%rax), %rax
	popq %rbx
	leaq (%rax,%rbx), %rax
	movq (%rax), %rax
	pushq %rax
	popq %rax
	popq %rbx
	cmpq %rax, %rbx
	jge L8
	pushq $0
	movq %rbp, %rax
	leaq 24(%rax), %rax
	movq (%rax), %rax
	pushq %rax
	movq %rbp, %rax
	movq 16(%rax), %rax
	leaq 24(%rax), %rax
	movq (%rax), %rax
	pushq %rax
	pushq %rbp
	callq swap
	addq $32, %rsp
L8:
	movq %rbp, %rax
	leaq 24(%rax), %rax
	movq (%rax), %rax
	pushq %rax
	pushq $1
	popq %rax
	addq %rax, (%rsp)
	movq %rbp, %rax
	movq 16(%rax), %rax
	pushq %rax
	callq min
	addq $16, %rsp
L7:
	leaveq
	retq
sort:
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
	movq %rbp, %rax
	leaq 24(%rax), %rax
	movq (%rax), %rax
	pushq %rax
	movq %rbp, %rax
	movq 16(%rax), %rax
	leaq -16(%rax), %rax
	movq (%rax), %rax
	pushq %rax
	popq %rax
	popq %rbx
	cmpq %rax, %rbx
	jge L9
	subq $16, %rsp
	movq %rbp, %rax
	leaq 24(%rax), %rax
	movq (%rax), %rax
	pushq %rax
	pushq $1
	popq %rax
	addq %rax, (%rsp)
	pushq %rbp
	callq min
	addq $16, %rsp
	movq %rbp, %rax
	leaq 24(%rax), %rax
	movq (%rax), %rax
	pushq %rax
	pushq $1
	popq %rax
	addq %rax, (%rsp)
	movq %rbp, %rax
	movq 16(%rax), %rax
	pushq %rax
	callq sort
	addq $16, %rsp
L9:
	leaveq
	retq
