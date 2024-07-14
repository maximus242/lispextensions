;; vfmadd132ps_mm256_fmadd_ps.scm
;; vfmadd132ps ymm0, ymm1, ymm2
;; c4 e2 75 98 c2
(add-to-load-path "modules")

(use-modules (asm)
             (bin)
             (linker)
             (hexdump)
             (rnrs bytevectors)
             (ice-9 format))

(define *code*
  '((asm fma (vfmadd132ps ymm0 ymm1 ymm2))))

;; Use linker-link to process the code
(define linked (linker-link (decls *code*)))

;; Ensure the linked result is processed by bin/resolve
(define resolved (bin/resolve linked))

;; Convert the resolved output to a bytevector
(define (resolved-to-bytevector resolved)
  (let ((bytevector (make-u8vector (length resolved))))
    (let loop ((lst resolved) (i 0))
      (if (null? lst)
          bytevector
          (begin
            (display (format #f "Setting bytevector index ~a to ~a\n" i (car lst)))
            (u8vector-set! bytevector i (car lst))
            (loop (cdr lst) (+ i 1)))))))

;; Remove the existing file if it exists
(when (file-exists? "binary_output.bin")
  (delete-file "binary_output.bin"))

;; Write the resolved binary data to a file
(let ((binary-data (resolved-to-bytevector resolved)))
  (call-with-output-file "binary_output.bin"
    (lambda (port)
      (put-bytevector port binary-data)))
  
  ;; Print the binary data in hexadecimal format
  (display "Generated binary output (hex):\n")
  (let loop ((i 0) (n (u8vector-length binary-data)))
    (when (< i n)
      (display (format #f "~02x " (u8vector-ref binary-data i)))
      (if (= (modulo (+ i 1) 16) 0) (newline))
      (loop (+ i 1) n)))
  (newline))

;; Output the resolved binary data using hexdump
(hexdump resolved)
