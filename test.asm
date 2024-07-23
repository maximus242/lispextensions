section .data
buffer1 db '12345678123456781234567812345678'
buffer2 db '87654321876543218765432187654321'
result  times 32 db 0
multiplier dd 0x40000000, 0x40000000, 0x40000000, 0x40000000
           dd 0x40000000, 0x40000000, 0x40000000, 0x40000000

fmt db '%02x', 0

section .text
global _start
extern printf

_start:
    ; Load addresses of buffers into registers
    lea rdi, [buffer1]
    lea rsi, [buffer2]
    lea rdx, [result]

    ; Load data from buffers into ymm registers
    vmovaps ymm0, [rdi]
    vmovaps ymm1, [rsi]

    ; Add the two buffers
    vaddps ymm2, ymm0, ymm1

    ; Multiply the result by a constant (e.g., 2.0)
    vmovaps ymm3, [multiplier]
    vfmadd132ps ymm2, ymm2, ymm3

    ; Store the result back into the result buffer
    vmovaps [rdx], ymm2

    ; Zero out the result buffer
    vxorps ymm2, ymm2, ymm2
    vmovaps [rdx + 32], ymm2

    ; Print result buffer
    mov rdi, result
    call print_result

    ; Exit
    mov eax, 60       ; syscall: exit
    xor edi, edi      ; status: 0
    syscall

print_result:
    ; Loop to print each byte in the result buffer
    mov rcx, 32        ; Number of bytes to print
print_loop:
    movzx rsi, byte [rdi] ; Load byte into rsi
    lea rdi, [fmt]     ; Load format string into rdi
    call printf        ; Call printf
    inc rdi            ; Move to next byte
    loop print_loop    ; Loop until rcx is 0
    ret
