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
	pushq $0
	popq %rax
	cmpq $0, %rax
	je else0
	pushq $string0
	jmp end0
else0:
	pushq $string1
end0:
	call log
	pushq %rax
	ret
	pushq $1
	popq %rax
	cmpq $0, %rax
	je else1
	pushq $string2
	jmp end1
else1:
	pushq $string3
end1:
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
	je else3
	pushq $string4
	jmp end3
else3:
	pushq $string5
end3:
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
	je else5
	pushq $string6
	jmp end5
else5:
	pushq $string7
end5:
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
string1:
	.string "h"
string2:
	.string "e"
string3:
	.string "f"
string4:
	.string "c"
string5:
	.string "d"
string6:
	.string "a"
string7:
	.string "b"
