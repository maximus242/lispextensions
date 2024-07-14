;; test-asm.scm
(add-to-load-path "modules")

(use-modules (asm))

(display "Testing asm...\n")
(let* ((code '((vxorps ymm0 ymm0 ymm0)))
       (assembled (asm code)))
  (display assembled)
  (newline))
