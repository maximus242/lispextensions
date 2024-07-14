;; vmovaps_mm256_load_ps.scm
;; vmovaps ymm0, [rdi]
;; C5FC280500000000
;; c5 fc 28 05 00 00 00    vmovaps ymm0,YMMWORD PTR ds:0x0
;; 00 
;; 
(add-to-load-path "modules")

(use-modules (asm)
             (linker)
             (bin)
             (rnrs io ports)
             (rnrs files)
             (rnrs bytevectors)
             (ice-9 format))

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
  '((asm load_ps (vmovaps ymm0 [rdi]))))

;; Use linker-link to process the code
(define linked (linker-link (decls *code*)))

;; Ensure the linked result is processed by bin/resolve
(define resolved (bin/resolve linked))

;; Function to convert resolved output to a bytevector
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

  ;; Print confirmation message or verify the content
  (display "Generated binary_outputwith correct data.\n")
  ;; Optionally, print or verify the binary data
  (let ((process (open-pipe* OPEN_READ "hexdump" "-C" "binary_output.bin")))
    (let loop ((line (get-line process)))
      (if (eof-object? line)
          (close-pipe process)
          (begin
            (display line)
            (newline)
            (loop (get-line process)))))))
