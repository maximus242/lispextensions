section .data
buffer1 db '12345678123456781234567812345678'
buffer2 db '87654321876543218765432187654321'
result times 32 db 0
multiplier times 8 dd 0x40000000  ; 2.0 in IEEE 754 floating-point

section .text
global _start

_start:
    ; Load addresses of buffers into registers
    lea rdi, [rel buffer1]
    lea rsi, [rel buffer2]
    lea rdx, [rel result]

    ; Load data from buffers into ymm registers
    vmovaps ymm0, [rdi]
    vmovaps ymm1, [rsi]

    ; Add the two buffers
    vaddps ymm2, ymm0, ymm1

    ; Multiply the result by a constant (2.0)
    lea rcx, [rel multiplier]
    vmovaps ymm3, [rcx]
    vmulps ymm2, ymm2, ymm3

    ; Store the result back into the result buffer
    vmovaps [rdx], ymm2

    ; Zero out the result buffer (next 16 bytes)
    vxorps ymm2, ymm2, ymm2
    vmovaps [rdx+16], ymm2

    ; Exit
    mov eax, 60   ; syscall: exit
    xor edi, edi  ; status: 0
    syscall
