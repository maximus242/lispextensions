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
    [`((symbolic ,sym))
     (let ((val (assoc-ref env sym)))
       (if val
           val
           (error "Undefined symbolic reference" sym)))]
    [`(str ,s)
     (map char->integer (string->list s))]
    [`(repeat ,n ,val)
     (let ((evaluated-val (subexpr val)))
       (apply append (make-list n (list evaluated-val))))]
    [err (error "Invalid Expression" err)]))

(define (bin/bytes len val out)
  (if (<= len 0) out
      (bin/bytes (- len 1) (floor (/ val 256))
                 (cons (modulo val 256) out))))

(define (assq-delete-all key alist)
  (filter (lambda (pair) (not (eq? (car pair) key))) alist))

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
     (let ((val (if (integer? x) x (bin/eval x off env))))
       (if (list? val)
           (bin/values (append val out)
                       (+ off (length val))
                       xs env)
           (bin/values (bin/bytes n val out)
                       (+ off n) xs env)))]
    [`((modrm ,mod ,reg ,rm) . ,xs)
     ;; Handle modrm, adding some placeholder processing if needed
     (bin/values out off xs env)]
    [`((byte ,b) . ,xs)
     ;; Handle byte, adding the byte to output
     (bin/values (cons b out) (+ off 1) xs env)]
    [`((symbolic ,sym) . ,xs)
     ;; Evaluate the symbolic reference
     (let ((val (assoc-ref env sym)))
       (if val
           (bin/values (cons val out) (+ off 1) xs env)
           (error "Undefined symbolic reference" sym)))]
    [err (error "Invalid Statement" err)]))

(define (bin/consts off stmts env)
  (match stmts
    [`() env]
    [`((const ,name ,expr) . ,xs)
     (if (assoc name env)
         (bin/consts off xs env)
         (let ((val (bin/eval expr off env)))
           (bin/consts off xs (acons name val env))))]
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
    [`((symbolic ,sym) . ,xs)
     ;; Handle symbolic references, ensuring they are resolved
     (let ((val (assoc-ref env sym)))
       (if val
           (bin/consts off xs env)
           (error "Undefined symbolic reference" sym)))]
    [err (error "Invalid Statement" err)]))

(define (bin/resolve stmts)
  (let ((initial-env (list (cons 'buffer1 0)
                           (cons 'buffer2 32)
                           (cons 'result 64)
                           (cons 'multiplier 96)
                           (cons 60 60))))
    (bin/values '() 0 stmts (bin/consts 0 stmts initial-env))))
