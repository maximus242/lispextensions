;; new-test-script.scm
(use-modules (system foreign))

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
