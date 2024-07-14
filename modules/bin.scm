;; modules/bin.scm
(define-module (bin)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (bin/eval bin/bytes bin/values bin/consts bin/resolve))

(define (bin/eval expr addr env)
  (define (subexpr x) (bin/eval x addr env))
  (match expr
    ['$ addr]
    [(? integer?) expr]
    [(? symbol?)
     (if (assoc expr env)
         (cdr (assoc expr env))
         (error "Not Defined" expr))]
    [`(+ . ,xs) (apply + (map subexpr xs))]
    [`(- . ,xs) (apply - (map subexpr xs))]
    [`(* . ,xs) (apply * (map subexpr xs))]
    [`(/ ,x ,y) (quotient (subexpr x) (subexpr y))]
    [`(% ,x ,y) (modulo (subexpr x) (subexpr y))]
    [err (error "Invalid Expression" err)]))

(define (bin/bytes len val out)
  (if (<= len 0) out
      (bin/bytes (- len 1) (floor (/ val 256))
                 (cons (modulo val 256) out))))

(define (bin/values out off stmts env)
  (match stmts
    [`() (reverse out)]
    [`((const ,name ,expr) . ,xs) (bin/values out off xs env)]
    [`((origin ,newoff) . ,xs) (bin/values out newoff xs env)]
    [`((align ,size) . ,xs)
     (let ((len (modulo (- 0 off) size)))
       (bin/values (bin/bytes len 0 out)
                   (+ off len) xs env))]
    [`((value ,n ,x) . ,xs)
     (let ([val (bin/eval x off env)])
       (bin/values (bin/bytes n val out)
                   (+ off n) xs env))]
    [`((modrm ,mod ,reg ,rm) . ,xs)
     ;; Handle modrm, adding some placeholder processing if needed
     (bin/values out off xs env)]
    [`((byte ,b) . ,xs)
     ;; Handle byte, adding the byte to output
     (bin/values (cons b out) (+ off 1) xs env)]
    [err (error "Invalid Statement" err)]))

(define (bin/consts off stmts env)
  (match stmts
    [`() env]
    [`((const ,name ,expr) . ,xs)
     (if (assoc name env)
         (error "Already Defined" name)
         (bin/consts off xs
                     (cons (cons name (bin/eval expr off env)) env)))]
    [`((origin ,newoff) . ,xs) (bin/consts newoff xs env)]
    [`((align ,size) . ,xs)
     (let ((len (modulo (- 0 off) size)))
       (bin/consts (+ off len) xs env))]
    [`((value ,n ,x) . ,xs) (bin/consts (+ off n) xs env)]
    [`((modrm ,mod ,reg ,rm) . ,xs)
     ;; Handle modrm, skipping it as it doesn't affect offset
     (bin/consts off xs env)]
    [`((byte ,b) . ,xs)
     ;; Handle byte, skipping it as it doesn't affect offset
     (bin/consts off xs env)]
    [err (error "Invalid Statement" err)]))

(define (bin/resolve stmts)
  (bin/values '() 0 stmts (bin/consts 0 stmts '())))
