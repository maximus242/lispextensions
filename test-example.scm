(add-to-load-path "modules")

(define-module (test module name)
  #:use-module (asm)
  #:use-module (bin)
  #:use-module (hexdump)
  #:use-module (linker)
  #:use-module (srfi srfi-64))

(test-begin "test-vaddps_mm256_add_ps")

(test-assert (equal?
	      '(197 244 88 194)
	      (bin/resolve (linker-link (decls '((asm add_ps (vaddps ymm0 ymm1 ymm2))))))))

(test-end "test-vaddps_mm256_add_ps")

(test-begin "test-vaddps_mm256_add_ps_with_real_numbers")

(let* ((init-ymm1 '(1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0))  ; Initial values for ymm1
       (init-ymm2 '(8.0 7.0 6.0 5.0 4.0 3.0 2.0 1.0))  ; Initial values for ymm2
       (expected '(9.0 9.0 9.0 9.0 9.0 9.0 9.0 9.0))  ; Expected result in ymm0
       (init-code (asm/init-ymm-registers '((ymm1 init-ymm1) (ymm2 init-ymm2))))
       (add-code (asm (vaddps ymm0 ymm1 ymm2)))
       (final-code (linker-link (decls `(,init-code ,add-code))))
       (result (bin/execute final-code)))
  
  (test-assert (equal? expected result)))

(test-end "test-vaddps_mm256_add_ps_with_real_numbers")
