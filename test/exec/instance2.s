	.text
	pushq $1
	call bar
	pushq %rax
	ret
	call log
	pushq %rax
	ret
	pushq $1
	call foo
	pushq %rax
	ret
	call log
	pushq %rax
	ret
	.data
