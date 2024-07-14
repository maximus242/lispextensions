;; Load necessary modules
(use-modules (ice-9 match)
             (srfi srfi-1))

;; Hexdump functions for displaying binary data
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

;; Binary Description Language (BIN)
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
    [err (error "Invalid Statement" err)]))

(define (bin/resolve stmts)
  (bin/values '() 0 stmts (bin/consts 0 stmts '())))

;; Linker functions
(define (concat xs) (apply append xs))
(define (catmap f xs) (concat (map f xs)))
(define (intersperse y xs)
  (cdr (apply append (map (lambda (x) (list y x)) xs))))
(define (symcat . xs)
  (string->symbol
    (string-concatenate (intersperse ":" (map symbol->string xs)))))

(define (link objs)
  (catmap link/flatten (link/elf-wrapper objs)))

(define (link/flatten obj)
  (match obj
    [`(object ,name ,type . ,body)
     `((const ,name $) ,@body (const ,(symcat name 'end) $))]
    [err (error "Invalid Object" err)]))

(define (link/elf-wrapper syms)
  "Wraps SYMS in a 32bit x86 ELF header."
  `((object %forig% data (origin #x08048000))
    (object %fbase% data)
    (object %elfhdr% data ,@(asm
      `((u8 127 69 76 70 1 1 1 3)  ;; ident[0:8]
        (u8 0 0 0 0 0 0 0 0)       ;; ident[8:16]
        (u16 2 3)                  ;; type/machine
        (u32 1 %start%)            ;; version/entry
        (u32 (- %elfpht% %fbase%)) ;; phoff
        (u32 0 0)                  ;; shoff/flags
        (u16 52 32 1)              ;; ehsize/phentsize/phnum
        (u16 0 0 0))))             ;; shentsize/shnum/shstrndx
    (object %elfpht% data ,@(asm
      `((u32 1 0)                   ;; type/offset
        (u32 %fbase% %fbase%)       ;; vaddr/paddr
        (u32 (- %flast% %fbase%))   ;; filesz
        (u32 (- %flast% %fbase%))   ;; memsz
        (u32 7 4096))))             ;; flags/align
    ,@syms
    (object %start% exec ,@(asm
      `((call init)
        (mov ebx eax)               ;; Exit Status
        (mov.imm32 eax 1)           ;; Syscall 1 (Exit)
        (int #x80))))
    (object %flast% data)))

;; Declarations and ASM
(define (decls xs)
  "Iteratively desugar declarations into OBJECTs."
  (define (step out xs)
    (match xs
      [(('object b ...) ys ...)
       (step (cons (car xs) out) ys)]
      [(d ys ...)
       (step out (append (decls/expand d) ys))]
      [_ (reverse out)]))
  (step '() xs))

(define (decls/expand decl)
  "Desugar a single declaration into simpler forms."
  (match decl
    [`(variable ,name)
     `((object ,name vars (value 4 0)))]
    [`(variable ,name ,val)
     `((object ,name vars (value 4 ,val)))]
    [('asm name body ...)
     `((object ,name code ,@(asm body)))]
    [('func name args body ...)
      (error "This is an Assembler, Not a Compiler!" name)]
    [err (error "Invalid Declaration" err)]))

(define (modrm mod reg rm)
  (let ((mod-val (case mod
                   [(#xC0) 192] ;; Direct register addressing
                   ;; Add other addressing modes if needed
                   [else (error "Invalid mod value" mod)]))
        (reg-val (asm/regix reg))
        (rm-val (asm/regix rm)))
    (if (and mod-val reg-val rm-val)
        (list (u8 (+ mod-val (* 8 reg-val) rm-val)))
        (error "Invalid modrm values" mod reg rm))))

(define (asm xs)
  (define (step out xs)
    (match xs
      [`((const ,n ,x) . ,ys) (step `((const ,n ,x) . ,out) ys)]
      [`((value ,n ,x) . ,ys) (step `((value ,n ,x) . ,out) ys)]
      [`(,inst . ,ys) (step out (append (asm/desugar inst) ys))]
      [_ (reverse out)]))
  (step '() xs))

(define (asm/regix reg)
  "Translates a symbolic register name into an index."
  (match reg
    ['ymm0 0] ['ymm1 1] ['ymm2 2] ['ymm3 3]
    ['ymm4 4] ['ymm5 5] ['ymm6 6] ['ymm7 7]
    [x (error "Invalid Register" reg)]))

(define (string->bytes str)
  (map char->integer (string->list str)))

(define (asm/desugar inst)
  "Decomposes a high-level instruction into a sequence of simpler instructions."
  (match inst
    ;; Handle vxorps instruction
    [`(vxorps ,dst ,src1 ,src2)
     (let ((dst-ix (asm/regix dst))
           (src1-ix (asm/regix src1))
           (src2-ix (asm/regix src2)))
       ;; Check if the registers are valid
       (if (and dst-ix src1-ix src2-ix)
           ;; Generate the correct bytecode for vxorps
           `((u8 #xC5) (u8 #xF8) (u8 #x57) (modrm #xC0 ,dst ,src1))
           (error "Invalid vxorps registers" dst src1 src2)))]
    ;; Handle other instructions
    [err (error "Invalid Instruction" err)]))

;; Top-Level Driver
(define *code*
  '((asm zero_ps (vxorps ymm0 ymm0 ymm0))))

(hexdump (bin/resolve (link (decls *code*))))
