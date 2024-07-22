;; simple_test.scm
(use-modules (system foreign))

;; Dynamically link the C standard library
(define libc (dynamic-link "libc.so.6"))

;; Get the 'puts' function from the C standard library
(define c-puts
  (pointer->procedure int (dynamic-func "puts" libc) (list (foreign-type "char*"))))

;; Call the 'puts' function
(c-puts "Hello, World!")
