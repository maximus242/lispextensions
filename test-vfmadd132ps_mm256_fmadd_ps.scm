;; test-vfmadd132ps_mm256_fmadd_ps.scm
(add-to-load-path "modules")

(use-modules (asm)
             (bin)
             (linker)
             (hexdump)
             (rnrs io ports)
             (rnrs files)
             (rnrs bytevectors)
             (ice-9 popen)
             (ice-9 rdelim)
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

;; Define resolved-to-bytevector
(define (resolved-to-bytevector resolved)
  (let ((bytevector (make-u8vector (length resolved))))
    (let loop ((lst resolved) (i 0))
      (if (null? lst)
          bytevector
          (begin
            (u8vector-set! bytevector i (car lst))
            (loop (cdr lst) (+ i 1)))))))

;; Main test function
(define (run-main-test)
  (display "Running vfmadd132ps_mm256_fmadd_ps.scm...\n")
  (load "vfmadd132ps_mm256_fmadd_ps.scm")
  (display "Finished running vfmadd132ps_mm256_fmadd_ps.scm\n")
  (display "Validating hash...\n")
  (let ((process (open-pipe* OPEN_READ "sh" "validate_hash.sh")))
    (let loop ((line (get-line process)))
      (if (eof-object? line)
          (begin
            (close-pipe process)
            (display "\nValidation completed.\n"))
          (begin
            (display line)
            (newline)
            (loop (get-line process)))))))

(run-main-test)
