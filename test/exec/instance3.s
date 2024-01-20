	.text
_start:
	pushq $42
	call bar
	pushq %rax
	ret
	call log
	pushq %rax
	ret
	.data
