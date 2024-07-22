(use-modules (system foreign))

(define libm (dynamic-link "libm.so.6"))

(define c-puts
  (dynamic-func "puts" libm))

(define (hello-world msg)
  ((foreign-lambda void "puts" (c-string)) msg))

(hello-world "Hello, World!")

