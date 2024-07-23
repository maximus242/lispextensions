section data
    buffer1 db "12345678123456781234567812345678"
    buffer2 db "87654321876543218765432187654321"
    result  times 32 db 0
    multiplier dq 0x40000000, 0x40000000, 0x40000000, 0x40000000, 0x40000000, 0x40000000, 0x40000000, 0x40000000

section text
    global _start

_start:
    ; Load addresses of buffers into registers
    mov rdi, buffer1
    mov rsi, buffer2
    mov rdx, result

    ; Load data from buffers into ymm registers
    vmovaps ymm0, [rdi]
    vmovaps ymm1, [rsi]

    ; Add the two buffers
    vaddps ymm2, ymm0, ymm1

    ; Multiply the result by a constant (e.g., 2.0)
    vmovaps ymm3, [multiplier]
    vmulps ymm2, ymm2, ymm3

    ; Store the result back into the result buffer
    vmovaps [rdx], ymm2

    ; Zero out the result buffer
    vxorps ymm2, ymm2, ymm2
    vmovaps [rdx + 32], ymm2

    ; Exit
    mov eax, 60          ; syscall: exit
    xor edi, edi         ; status: 0
    syscall
