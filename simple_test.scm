(use-modules (system foreign) (system base types))

(define hello-world
  (foreign-lambda void "puts" (c-string)))

(hello-world "Hello, World!")
