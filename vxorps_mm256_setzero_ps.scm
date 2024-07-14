;; yxorps_mm256_set_zero.scm
;; vxorps ymm0, ymm0, ymm0
;; c5 fc 57 c0
(add-to-load-path "modules")

(use-modules (hexdump)
             (bin)
             (linker)
             (asm)
             (rnrs io ports)
             (rnrs files)
             (rnrs bytevectors)
             (ice-9 format)) ;; Import ice-9 format for formatting

;; Define bytevector-append
(define (bytevector-append . bvs)
  (let* ((total-length (apply + (map bytevector-length bvs)))
         (result (make-bytevector total-length)))
    (let loop ((offset 0) (remaining bvs))
      (if (null? remaining)
          result
          (let* ((bv (car remaining))
                 (len (bytevector-length bv)))
            (bytevector-copy! result offset bv 0 len)
            (loop (+ offset len) (cdr remaining)))))))

(define *code*
  '((asm zero_ps (vxorps ymm0 ymm0 ymm0))))

;; Use linker-link to process the code
(define linked (linker-link (decls *code*)))

;; Ensure the linked result is processed by bin/resolve
(define resolved (bin/resolve linked))

;; Convert the resolved output to a bytevector
(define (resolved-to-bytevector resolved)
  (let ((bytevector (make-bytevector (length resolved))))
    (let loop ((lst resolved) (i 0))
      (if (null? lst)
          bytevector
          (begin
            (bytevector-u8-set! bytevector i (car lst))
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
  (let loop ((i 0) (n (bytevector-length binary-data)))
    (when (< i n)
      (display (format #f "~02x " (bytevector-u8-ref binary-data i)))
      (if (= (modulo (+ i 1) 16) 0) (newline))
      (loop (+ i 1) n)))
  (newline))

;; Output the resolved binary data using hexdump
(hexdump resolved)
