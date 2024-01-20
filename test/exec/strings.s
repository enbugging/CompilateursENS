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
	pushq $string0
	call log
	pushq %rax
	ret
	pushq $string0
	call log
	pushq %rax
	ret
	pushq $string0
	call log
	pushq %rax
	ret
	pushq $string0
	call log
	pushq %rax
	ret
	pushq $string0
	call log
	pushq %rax
	ret
	pushq $string0
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
	.string "hello world"
string0:
	.string "hello world"
string0:
	.string "hello \"world\""
string0:
	.string "hello world"
string0:
	.string "hello \nworld"
string0:
	.string "hello world"
