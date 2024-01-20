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
	call not
	pushq %rax
	ret
	call show_bool
	pushq %rax
	ret
	call log
	pushq %rax
	ret
	pushq $1
	call not
	pushq %rax
	ret
	call show_bool
	pushq %rax
	ret
	call log
	pushq %rax
	ret
	pushq $1
	pushq $2
	popq %rbx
	popq %rax
	cmpq %rbx, %rax
	setl %al
	movzbq %al, %rax
	pushq %rax
	call not
	pushq %rax
	ret
	call not
	pushq %rax
	ret
	popq %rax
	cmpq $0, %rax
	je else0
	pushq $string0
	jmp end0
else0:
	pushq $string0
end0:
	call log
	pushq %rax
	ret
	pushq $1
	pushq $2
	popq %rbx
	popq %rax
	cmpq %rbx, %rax
	setl %al
	movzbq %al, %rax
	pushq %rax
	call not
	pushq %rax
	ret
	popq %rax
	cmpq $0, %rax
	je else0
	pushq $string0
	jmp end0
else0:
	pushq $string0
end0:
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
string0:
	.string "a"
string0:
	.string "b"
string0:
	.string "a"
string0:
	.string "b"
