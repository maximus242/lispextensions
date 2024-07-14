;; test-bin.scm
(add-to-load-path "modules")

(use-modules (bin))

(display "Testing bin...\n")
(let* ((stmts '((const one 1)
                (value 1 one)))
       (resolved (bin/resolve stmts)))
  (display resolved)
  (newline))
