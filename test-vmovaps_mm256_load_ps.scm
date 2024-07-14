;; test-vmovaps_mm256_load_ps.scm
(add-to-load-path "modules")
(use-modules (rnrs io ports)
             (rnrs files)
             (ice-9 popen)
             (ice-9 rdelim)) ;; Import rdelim for get-line

;; Run load_ps.scm to generate the binary output
(define (load-and-run script)
  (load script))

;; Main test function
(define (run-main-test)
  (display "Running vmovaps_mm256_load_ps.scm...\n")
  (load-and-run "vmovaps_mm256_load_ps.scm")
  (display "Finished running vmovaps_mm256_load_ps.scm\n")
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
