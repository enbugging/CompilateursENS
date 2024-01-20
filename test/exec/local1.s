	.text
log:
	popq %rsi
	movq $string_format, %rdi
	xorq %rax, %rax
	call printf
	ret
show_int:
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
f_l_Unit_r_l_Effect_l_Unit_r_r:
	pushq $0
_start:
	pushq $0
	.data
string_format:
	.string "%s\n"
show_int_format:
	.string "%lld"
true:
	.string "true"
false:
	.string "false"
