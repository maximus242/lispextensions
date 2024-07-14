;; test-vaddps.scm
(add-to-load-path "modules")

(use-modules (asm))

(define *code*
  '((asm add_ps (vaddps ymm0 ymm1 ymm2))))

;; Attempt to desugar the instruction
(define desugared (asm/desugar '(vaddps ymm0 ymm1 ymm2)))
(display desugared)
(newline)
