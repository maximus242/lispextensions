;; modules/bin.scm
(define-module (bin)
               #:use-module (ice-9 match)
               #:use-module (srfi srfi-1)
               #:use-module (rnrs bytevectors)
               #:export (bin/eval bin/bytes bin/values bin/consts bin/resolve))

(define (bin/eval expr addr env)
  (define (subexpr x) (bin/eval x addr env))
  (display (format #f "Evaluating expression: ~a\n" expr))
  (match expr
         ['$ addr]
         [(? integer?) expr]
         [(? symbol?)
          (let ((val (assoc expr env)))
            (if val
              (cdr val)
              (error "Not Defined" expr)))]
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
         ;; Pattern for bytevector
         [(? bytevector? expr) (bytevector->u8-list expr)]
         [err (error "Invalid Expression" err)]))

(define (bin/bytes len val out)
  (if (<= len 0) out
    (bin/bytes (- len 1) (floor (/ val 256))
               (cons (modulo val 256) out))))

(define (assq-delete-all key alist)
  (filter (lambda (pair) (not (eq? (car pair) key))) alist))

(define (my-type obj)
  (cond
    ((pair? obj) 'pair)
    ((null? obj) 'null)
    ((symbol? obj) 'symbol)
    ((number? obj) 'number)
    ((string? obj) 'string)
    ((vector? obj) 'vector)
    ((procedure? obj) 'procedure)
    (else 'unknown)))

(define (bin/consts off stmts env)
  (match stmts
    ;; Base case: empty statements list
    [`()
     (begin
       (display (format #f "Final environment: ~a\n" env))
       (cons off env))]

    ;; Handle constant definition
    [`((const ,name ,expr) . ,xs)
     (begin
       (display (format #f "Adding const: ~a ~a\n" name expr))
       (if (assoc name env)
         (begin
           (display (format #f "Skipping redefinition of: ~a\n" name))
           (bin/consts off xs env))
         (let* ((val (bin/eval expr off env))
                (new-env (acons name val env)))
           (display (format #f "Evaluated const: ~a = ~a\n" name val))
           (bin/consts off xs new-env))))]

    ;; Handle origin change
    [`((origin ,newoff) . ,xs)
     (begin
       (display (format #f "Changing origin to: ~a\n" newoff))
       (bin/consts newoff xs env))]

    ;; Handle alignment
    [`((align ,size) . ,xs)
     (begin
       (let ((len (modulo (- size (modulo off size)) size)))
         (display (format #f "Aligning to size: ~a, padding with: ~a bytes\n" size len))
         (bin/consts (+ off len) xs env)))]

    ;; Handle value statements
    [`((value ,n ,x) . ,xs)
     (begin
       (let* ((val (bin/eval x off env))
              (new-off (+ off (if (list? val) (length val) n))))
         (display (format #f "Adding value: ~a ~a (evaluated to: ~a)\n" n x val))
         (bin/consts new-off xs env)))]

    ;; Handle symbolic references
    [`((symbolic ,sym) . ,xs)
     (begin
       (let ((val (assoc-ref env sym)))
         (display (format #f "Processing symbolic reference: ~a (resolved to: ~a)\n" sym val))
         (if val
             (bin/consts (+ off 4) xs env)
             (error "Undefined symbolic reference" sym))))]
             
    ;; Handle single byte
    [`((byte ,b) . ,xs)
     (begin
       (display (format #f "Adding byte: ~a\n" b))
       (bin/consts (+ off 1) xs env))]

    ;; Handle nested byte lists
    [`((byte . ,bytes) . ,xs)
     (begin
       (display (format #f "Processing bytes: ~a\n" bytes))
       (let loop ((b bytes) (new-off off))
         (if (null? b)
             (bin/consts new-off xs env)
             (let ((byte (car b)))
               (display (format #f "Adding nested byte: ~a\n" byte))
               (loop (cdr b) (+ new-off 1))))))]

    ;; Handle u8, u16, u32, etc.
    [`((,type ,val) . ,xs)
     (begin
       (cond
         ((eq? type 'u8) (bin/consts off `((value 1 ,val) . ,xs) env))
         ((eq? type 'u16) (bin/consts off `((value 2 ,val) . ,xs) env))
         ((eq? type 'u32) (bin/consts off `((value 4 ,val) . ,xs) env))
         ((eq? type 'u64) (bin/consts off `((value 8 ,val) . ,xs) env))
         (else (error "Undefined symbol" type))))]

    ;; Handle vals pattern
    [`((vals ,n ,x . ,xs) . ,rest)
     (bin/consts off `((value ,n ,x) (vals ,n . ,xs) . ,rest) env)]

    [`((vals ,n) . ,rest)
     (bin/consts off rest env)]

    ;; Handle complex nested statements
    [(lst . rest)
     (cond
       ((list? lst)
        (display (format #f "Handling nested list: ~a\n" lst))
        (let ((expanded-lst (bin/consts off lst env)))
          (bin/consts (car expanded-lst) rest (cdr expanded-lst))))
       ((symbol? lst)
        (match rest
               ;; Handle specific vals pattern
               [`((,n ,m ,sym) . ,rest)
                (bin/consts off `((value ,n ,sym) . ,rest) env)]
               ;; Handle other vals patterns
               [else
                (let ((val (assoc-ref env lst)))
                  (if val
                      (bin/consts off (cons val rest) env)
                      (error "Undefined symbol" lst)))]))
       (else
        (error "Unexpected non-list, non-symbol structure" lst)))]

    ;; Catch-all pattern for invalid statements
    [else
     (begin
       (display (format #f "Invalid statement encountered: ~a\n" stmts))
       (error "Invalid Statement" stmts))]))

(define (bin/bytes len val out)
  (if (<= len 0) out
    (bin/bytes (- len 1) (floor (/ val 256))
               (cons (modulo val 256) out))))

(define (flatten lst)
  (if (null? lst)
    '()
    (if (list? (car lst))
      (append (flatten (car lst)) (flatten (cdr lst)))
      (cons (car lst) (flatten (cdr lst))))))

(define (bin/values out off stmts env)
  ;; Ensure offset is numeric
  (let ((off (if (number? off) off 0)))
    ;; Convert out to an empty list if it is not a list
    (if (not (list? out)) 
      (begin
        (display (format #f "Out variable is not a list. Converting to an empty list: ~a\n" out))
        (set! out '())))
    (display (format #f "Processing values with offset ~a: ~a\n" off stmts))
    (display (format #f "Out variable before processing: ~a\n" out))
    (match stmts
           [`() (reverse out)]

           ;; Handle constant definition
           [`((const ,name ,expr) . ,xs)
             (bin/values out off xs env)]

           ;; Handle origin change
           [`((origin ,newoff) . ,xs)
             (bin/values out newoff xs env)]

           ;; Handle alignment
           [`((align ,size) . ,xs)
             (let ((len (modulo (- size (modulo off size)) size)))
               (let ((new-out (append out (bin/bytes len 0 '()))))
                 (display (format #f "Aligning: new out variable: ~a\n" new-out))
                 (bin/values new-out (+ off len) xs env)))]

           ;; Handle value statements
           [`((value ,n ,x) . ,xs)
             (let* ((val (bin/eval x off env))
                    (flat-val (flatten val))
                    (new-out (if (list? val) (append out flat-val) (append out (bin/bytes n val '()))))
                    (new-off (+ off (if (list? val) (length flat-val) n))))
               (display (format #f "Adding value: ~a ~a (evaluated to: ~a) with new offset ~a\n" n x val new-off))
               (display (format #f "New out variable after adding value: ~a\n" new-out))
               (bin/values new-out new-off xs env))]

           ;; Handle vals pattern
           [`((vals ,n ,x . ,xs) . ,rest)
             (bin/values out off `((value ,n ,x) (vals ,n . ,xs) . ,rest) env)]

           [`((vals ,n) . ,rest)
             (bin/values out off rest env)]

           ;; Handle symbolic references
           [`((symbolic ,sym) . ,xs)
             (let ((val (assoc-ref env sym)))
               (if val
                 (let* ((symbolic-val (if (number? val) val (error "Symbolic value is not numeric" val)))
                        (bytes (bin/bytes 4 symbolic-val '())))
                   (display (format #f "Processing symbolic reference: ~a (resolved to: ~a) with offset ~a\n" sym symbolic-val off))
                   (display (format #f "Bytes for symbolic reference: ~a\n" bytes))
                   (let ((new-out (append out bytes)))
                     (display (format #f "New out variable after processing symbolic reference: ~a\n" new-out))
                     (bin/values new-out (+ off 4) xs env)))
                 (error "Undefined symbolic reference" sym)))]

           ;; Handle single byte
           [`((byte ,b) . ,xs)
             (let ((new-out (append out (list b))))
               (display (format #f "New out variable after adding byte: ~a\n" new-out))
               (bin/values new-out (+ off 1) xs env))]

           ;; Handle nested byte lists
           [`((byte . ,bytes) . ,xs)
             (let loop ((b bytes) (new-out out) (new-off off))
               (if (null? b)
                 (bin/values new-out new-off xs env)
                 (let ((byte (car b)))
                   (let ((updated-out (append new-out (list byte))))
                     (display (format #f "New out variable in loop after adding nested byte: ~a\n" updated-out))
                     (loop (cdr b) updated-out (+ new-off 1))))))]

           ;; Handle complex nested statements
           [(lst . rest)
            (if (list? lst)
              (let ((result (bin/values out off lst env)))
                (bin/values (car result) (cdr result) rest env))
              (error "Unexpected non-list structure" lst))]

           ;; Catch-all pattern for invalid statements
           [else
             (error "Invalid Statement" stmts)])))

(define (bin/resolve stmts)
  (let ((initial-env (list (cons 'buffer1 0)
                           (cons 'buffer2 32)
                           (cons 'result 64)
                           (cons 'multiplier 96)
                           (cons '%start% 0)   ;; Adding %start% to the initial environment
                           (cons '%elfpht% 0)  ;; Adding %elfpht% to the initial environment
                           (cons '%fbase% 0)   ;; Adding %fbase% to the initial environment
                           (cons '%flast% 0)   ;; Adding %flast% to the initial environment
                           (cons 60 60))))
    (let ((result (bin/consts 0 stmts initial-env)))
      ;; Ensure out is initialized to an empty list
      (bin/values '() (car result) stmts (cdr result)))))
