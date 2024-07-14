;; test-linker.scm
(use-modules (linker))

(display "Testing linker...\n")
(let* ((objects '((object test data (value 4 42))))
       (linked (linker-link objects)))
  (display (format #f "Linked objects: ~a\n" linked)))
