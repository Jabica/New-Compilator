	.build_version macos, 15, 0
	.section	__TEXT,__text,regular,pure_instructions
	.globl	_main
	.p2align	2
_main:
	.cfi_startproc
	mov	w0, #7
	ret
	.cfi_endproc

.subsections_via_symbols
