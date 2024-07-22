(add-to-load-path "modules")

(define-module (test module name)
  #:use-module (asm)
  #:use-module (bin)
  #:use-module (hexdump)
  #:use-module (linker)
  #:use-module (srfi srfi-64))

;; Define the *code* variable correctly
(define *code*
  '((asm add_ps (vaddps ymm0 ymm1 ymm2))
    (asm broadcast_ps (vbroadcastf128 ymm0 rdi))
    (asm fma (vfmadd132ps ymm0 ymm1 ymm2))
    (asm load_ps (vmovaps ymm0 [rdi]))
    (asm zero_ps (vxorps ymm0 ymm0 ymm0))))

;; Resolve and link the code
(hexdump (bin/resolve (linker-link (decls *code*))))
