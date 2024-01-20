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
	pushq $3
	pushq $4
	popq %rbx
	popq %rax
	cmpq %rbx, %rax
	setg %al
	movzbq %al, %rax
	pushq %rax
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
	.string "g"
string0:
	.string "h"
string0:
	.string "e"
string0:
	.string "f"
string0:
	.string "c"
string0:
	.string "d"
string0:
	.string "a"
string0:
	.string "b"
