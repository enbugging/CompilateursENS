	.text
	popq %rsi
	movq $string_format, %rdi
	xorq %rax, %rax
	call printf
	ret
	movq 8(%rsi), %r15
	movq $20, %rdi
	call malloc
	movq %r15, %rdi
	movq $show_int_format, %rsi
	xorq %rax, %rax
	call sprintf
	ret
show_bool:
	testq %rax, %rax
	je show_true
	movq $false, %rax
	ret
show_true:
	movq $true, %rax
	ret
	pushq $0
	pushq $0
	pushq $34
	popq %rbx
	popq %rax
	subq %rbx, %rax
	pushq %rax
	popq %rbx
	popq %rax
	subq %rbx, %rax
	pushq %rax
	call show
	pushq %rax
	ret
	call log
	pushq %rax
	ret
	pushq $20
	pushq $0
	pushq $1
	popq %rbx
	popq %rax
	subq %rbx, %rax
	pushq %rax
	popq %rbx
	popq %rax
	subq %rbx, %rax
	pushq %rax
	call show
	pushq %rax
	ret
	call log
	pushq %rax
	ret
	pushq $2
	pushq $12
	pushq $143
	call mod
	pushq %rax
	ret
	popq %rbx
	popq %rax
	addq %rbx, %rax
	pushq %rax
	call show
	pushq %rax
	ret
	call log
	pushq %rax
	ret
	pushq $7
	pushq $2
	pushq $1
	call mod
	pushq %rax
	ret
	popq %rbx
	popq %rax
	addq %rbx, %rax
	pushq %rax
	call show
	pushq %rax
	ret
	call log
	pushq %rax
	ret
	pushq $75
	pushq $5
	popq %rbx
	popq %rax
	cqto
	idivq %rbx
	pushq %rax
	pushq $3
	popq %rbx
	popq %rax
	cqto
	idivq %rbx
	pushq %rax
	call show
	pushq %rax
	ret
	call log
	pushq %rax
	ret
	pushq $1
	pushq $1
	popq %rbx
	popq %rax
	imulq %rbx, %rax
	pushq %rax
	pushq $3
	popq %rbx
	popq %rax
	imulq %rbx, %rax
	pushq %rax
	call show
	pushq %rax
	ret
	call log
	pushq %rax
	ret
	pushq $5
	pushq $2
	popq %rbx
	popq %rax
	cqto
	idivq %rbx
	pushq %rax
	call show
	pushq %rax
	ret
	call log
	pushq %rax
	ret
	pushq $1
	pushq $2
	pushq $0
	popq %rbx
	popq %rax
	imulq %rbx, %rax
	pushq %rax
	popq %rbx
	popq %rax
	addq %rbx, %rax
	pushq %rax
	call show
	pushq %rax
	ret
	call log
	pushq %rax
	ret
	pushq $0
	pushq $1
	popq %rbx
	popq %rax
	addq %rbx, %rax
	pushq %rax
	call show
	pushq %rax
	ret
	call log
	pushq %rax
	ret
	pushq $1
	pushq $1
	popq %rbx
	popq %rax
	subq %rbx, %rax
	pushq %rax
	call show
	pushq %rax
	ret
	call log
	pushq %rax
	ret
	.data
string_format:
	.string "%s\n"
show_int_format:
	.string "%lld"
true:
	.string "true"
false:
	.string "false"
