.section .data
.align 32
buffer1:
    .ascii "12345678123456781234567812345678"
buffer2:
    .ascii "87654321876543218765432187654321"
result:
    .space 32  # Allocate 32 bytes (256 bits) for the result
multiplier:
    .float 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0  # Eight 2.0 floats (256 bits total)

.section .text
.globl _start
_start:
    # Load addresses of buffers into registers
    lea rdi, buffer1
    lea rsi, buffer2
    lea rdx, result

    # Load data from buffers into YMM registers
    vmovaps ymm0, [rdi]
    vmovaps ymm1, [rsi]

    # Add the two buffers
    vaddps ymm2, ymm0, ymm1

    # Multiply the result by the constant (2.0)
    vmovaps ymm3, multiplier
    vfmadd132ps ymm2, ymm2, ymm3

    # Store the result back into the result buffer
    vmovaps [rdx], ymm2

    # Zero out the result buffer
    vxorps ymm2, ymm2, ymm2
    vmovaps [rdx], ymm2

    # Exit
    mov eax, 60          # syscall: exit
    xor edi, edi         # status: 0
    syscall
