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
