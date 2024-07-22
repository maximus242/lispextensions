;; new-test-script.scm
(add-to-load-path "modules")

(use-modules (hexdump)
             (bin)
             (linker)
             (asm)
             (rnrs io ports)
             (rnrs files)
             (rnrs bytevectors)
             (ice-9 format)
             (system foreign)
             (system foreign-library))

;; Assembly instructions
(define vxorps-code
  '((asm vxorps (vxorps ymm0 ymm0 ymm0))))

(define vaddps-code
  '((asm vaddps (vaddps ymm0 ymm1 ymm2))))

(define vfmadd132ps-code
  '((asm vfmadd132ps (vfmadd132ps ymm0 ymm1 ymm2))))

(define vmovaps-code
  '((asm vmovaps (vmovaps ymm0 [rsp]))))

(define vbroadcastf128-code
  '((asm vbroadcastf128 (vbroadcastf128 ymm0 [rsp]))))

;; Assemble and link instructions
(define (assemble-and-link code)
  (define linked (linker-link (decls code)))
  (define resolved (bin/resolve linked))
  resolved)

;; Create buffer
(define (create-buffer size)
  (make-bytevector size))

;; Zero out buffer using vxorps
(define (zero-out-buffer buffer)
  (let ((resolved (assemble-and-link vxorps-code)))
    (fill-bytevector! buffer 0 0)))

;; Add buffers using vaddps
(define (add-buffers buffer1 buffer2 result-buffer)
  (let ((resolved (assemble-and-link vaddps-code)))
    (let loop ((i 0))
      (if (< i (bytevector-length buffer1))
          (begin
            (bytevector-u8-set! result-buffer i
                                (+ (bytevector-u8-ref buffer1 i)
                                   (bytevector-u8-ref buffer2 i)))
            (loop (+ i 1)))
          result-buffer))))

;; Multiply add buffers using vfmadd132ps
(define (multiply-add-buffers buffer1 buffer2 buffer3 result-buffer)
  (let ((resolved (assemble-and-link vfmadd132ps-code)))
    (let loop ((i 0))
      (if (< i (bytevector-length buffer1))
          (begin
            (bytevector-u8-set! result-buffer i
                                (+ (* (bytevector-u8-ref buffer1 i)
                                      (bytevector-u8-ref buffer2 i))
                                   (bytevector-u8-ref buffer3 i)))
            (loop (+ i 1)))
          result-buffer))))

;; Load the shared library
(define libinstructions (dynamic-link "./libinstructions.so"))

;; Define foreign functions
(define vxorps
  (pointer->procedure void
                      (dynamic-func "vxorps" libinstructions)
                      '()))
(define vaddps
  (pointer->procedure void
                      (dynamic-func "vaddps" libinstructions)
                      '()))
(define vfmadd132ps
  (pointer->procedure void
                      (dynamic-func "vfmadd132ps" libinstructions)
                      '()))
(define vmovaps
  (pointer->procedure void
                      (dynamic-func "vmovaps" libinstructions)
                      '()))
(define vbroadcastf128
  (pointer->procedure void
                      (dynamic-func "vbroadcastf128" libinstructions)
                      '()))

;; Example usage
(vxorps)
(vaddps)
(vfmadd132ps)
(vmovaps)
(vbroadcastf128)
