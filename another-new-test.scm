(add-to-load-path "modules")

(define-module (test module name)
  #:use-module (asm)
  #:use-module (bin)
  #:use-module (hexdump)
  #:use-module (linker)
  #:use-module (srfi srfi-64))

(define *code*
  '((asm init
         ;; Load addresses of buffers into registers
         (mov.imm32 rdi buffer1)
         (mov.imm32 rsi buffer2)
         (mov.imm32 rdx result)
         ;; Load data from buffers into ymm registers
         (vmovaps ymm0 (rdi))
         (vmovaps ymm1 (rsi))
         ;; Add the two buffers
         (vaddps ymm2 ymm0 ymm1)
         ;; Multiply the result by a constant (e.g., 2.0)
         (vmovaps ymm3 (multiplier))
         (vfmadd132ps ymm2 ymm2 ymm3)
         ;; Store the result back into the result buffer
         (vmovaps (rdx) ymm2)
         ;; Zero out the result buffer
         (vxorps ymm2 ymm2 ymm2)
         (vmovaps (rdx) ymm2)
         ;; Exit
         (mov.imm32 eax 60)  ;; syscall: exit
         (xor edi edi)       ;; status: 0
         (syscall))

    (asm buffer1
         (value 32 (str "12345678123456781234567812345678")))
    (asm buffer2
         (value 32 (str "87654321876543218765432187654321")))
    (asm result
         (value 32 (repeat 32 0)))
    (asm multiplier
         (value 32 (repeat 8 0x40000000))) ;; 2.0 in IEEE 754 floating-point
    ))

(hexdump (bin/resolve (linker-link (decls *code*))))
