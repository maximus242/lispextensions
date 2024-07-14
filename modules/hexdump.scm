;; modules/hexdump.scm
(define-module (hexdump)
  #:use-module (srfi srfi-1)
  #:export (chunk hex8 hexline hexdump))

(define (chunk n xs)
  (cond
    [(null? xs) '()]
    [(> n (length xs)) (list xs)]
    [#t (cons (take xs n) (chunk n (drop xs n)))]))

(define (hex8 x)
  (set! x (number->string (+ 256 x) 16))
  (set! x (string-append "00" x))
  (substring x (- (string-length x) 2)))

(define (hexline xs)
  (string-append (string-concatenate (map hex8 xs)) "\n"))

(define (hexdump xs)
  (display (string-concatenate (map hexline (chunk 16 xs)))))
