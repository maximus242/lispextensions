(use-modules (ice-9 match)
             (srfi srfi-1))

;; =================================
;; Printing Binary Data as a Hexdump
;; =================================

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

;; ================================
;; BIN: Binary Description Language
;; ================================

(define (bin/eval expr addr env)
  "Resolve a value expression into an actual number."
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
  "Expand VAL into LEN bytes (little-endian)."
  (if (<= len 0) out
      (bin/bytes (- len 1) (floor (/ val 256))
                 (cons (modulo val 256) out))))

(define (bin/values out off stmts env)
  "Assemble primitive statements into a list of bytes."
  (match stmts
    [`() (reverse out)]
    [`((const ,name ,expr) . ,xs) (bin/values out off xs env)]
    [`((origin ,newoff) . ,xs) (bin/values out newoff xs env)]
    [`((align ,size) . ,xs)
     (let ((len (modulo (- size (modulo off size)) size)))
       (bin/values (bin/bytes len 0 out)
                   (+ off len) xs env))]
    [`((value ,n ,x) . ,xs)
     (let ([val (bin/eval x off env)])
       (bin/values (bin/bytes n val out)
                   (+ off n) xs env))]
    [err (error "Invalid Statement" err)]))

(define (bin/consts off stmts env)
  "Accumulate a mapping from constant names to offsets."
  (match stmts
    [`() env]
    [`((const ,name ,expr) . ,xs)
     (if (assoc name env)
         (error "Already Defined" name)
         (bin/consts off xs
                     (cons (cons name (bin/eval expr off env)) env)))]
    [`((origin ,newoff) . ,xs) (bin/consts newoff xs env)]
    [`((align ,size) . ,xs)
     (let ((len (modulo (- size (modulo off size)) size)))
       (bin/consts (+ off len) xs env))]
    [`((value ,n ,x) . ,xs) (bin/consts (+ off n) xs env)]
    [err (error "Invalid Statement" err)]))

(define (bin/resolve stmts)
  (bin/values '() 0 stmts (bin/consts 0 stmts '())))

;; =============================================
;; LINK: Combining and Renaming Chunks of Binary
;; =============================================

(define (concat xs) (apply append xs))
(define (catmap f xs) (concat (map f xs)))
(define (intersperse y xs)
  (cdr (apply append (map (lambda (x) (list y x)) xs))))
(define (symcat . xs)
  "Concatenate symbols with ':' in between"
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

;; ============================
;; DECL: Top-Level Declarations
;; ============================

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

;; =======================================
;; ASM: Translating the x86 ISA into Bytes
;; =======================================

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
    ['eax 0] ['ecx 1] ['edx 2] ['ebx 3]
    ['esp 4] ['ebp 5] ['esi 6] ['edi 7]
    ['rdi 7] ['rsi 6] ['rdx 2] ['rax 0]
    ['ymm0 0] ['ymm1 1] ['ymm2 2] ['ymm3 3]
    ['ymm4 4] ['ymm5 5] ['ymm6 6] ['ymm7 7]
    [x (error "Invalid Register" reg)]))

(define (string->bytes str)
  (map char->integer (string->list str)))

(define (asm/desugar inst)
  "Decomposes a high-level instruction into a sequence of simpler instructions."
  (match inst
    ;; Data Literals
    [`(vals ,n) `()]
    [`(vals ,n ,x . ,xs) `((value ,n ,x) (vals ,n . ,xs))]
    [`(u8 . ,xs)  `((vals 1 . ,xs))]
    [`(u16 . ,xs) `((vals 2 . ,xs))]
    [`(u32 . ,xs) `((vals 4 . ,xs))]
    [`(u64 . ,xs) `((vals 8 . ,xs))]
    [`(str ,x) `((u8 . ,(string->bytes x)) (u8 0))]
    [`(repeat 0 ,x) `()]
    [`(repeat ,n . ,xs) `(,@xs (repeat ,(- n 1) . ,xs))]
    [`(label ,n) `((const ,n $))]
    ;; Instruction "Pieces"
    [`(disp ,addr)       `((u32 (- ,addr (+ $ 4))))]
    [`(disp.short ,addr) `((u8 (- ,addr (+ $ 1))))]
    [`(modrm ,mod ,rm) `((u8 (+ ,mod ,(asm/regix rm))))]
    [`(modrm ,mod ,reg ,rm)
     `((u8 (+ ,mod (* ,(asm/regix reg) 8) ,(asm/regix rm))))]
    ;; Instructions
    [`(nop) `((u8 #x90))]
    [`(cdq) `((u8 #x99))]
    [`(inc ,rd) `((u8 (+ #x40 ,(asm/regix rd))))]
    [`(dec ,rd) `((u8 (+ #x48 ,(asm/regix rd))))]
    [`(mov.imm32 ,rd ,x)
     `((u8 (+ #xB8 ,(asm/regix rd))) (u32 ,x))]
    [`(add.imm32 ,rd ,n)
     `((u8 #x81) (modrm #xC0 ,rd) (u32 ,n))]
    [`(mov.ld32 ,rd esp)
     `((u8 #x8B) (modrm #x00 ,rd esp) (u8 #x24))]
    [`(mov.st32 esp ,rs)
     `((u8 #x89) (modrm #x00 ,rs esp) (u8 #x24))]
    [`(mov.ld32 ,rd ebp) `((mov.ld32 ,rd (+ ebp 0)))]
    [`(mov.st32 ebp ,rs) `((mov.st32 (+ ebp 0) ,rs))]
    [`(mov.ld32 ,rd (+ ebp ,n))
     `((u8 #x8B) (modrm #x40 ,rd ebp) (u8 ,n))]
    [`(mov.st32 (+ ebp ,n) ,rs)
     `((u8 #x89) (modrm #x40 ,rs ebp) (u8 ,n))]
    [`(mov.ld32 ,rd ,rs) `((u8 #x8B) (modrm #x00 ,rd ,rs))]
    [`(mov.st32 ,rd ,rs) `((u8 #x89) (modrm #x00 ,rs ,rd))]
    [`(mov ,rd ,rs)      `((u8 #x8B) (modrm #xC0 ,rd ,rs))]
    [`(add ,rd ,rs)      `((u8 #x03) (modrm #xC0 ,rd ,rs))]
    [`(sub ,rd ,rs)      `((u8 #x2B) (modrm #xC0 ,rd ,rs))]
    [`(and ,rd ,rs)      `((u8 #x23) (modrm #xC0 ,rd ,rs))]
    [`( or ,rd ,rs)      `((u8 #x0B) (modrm #xC0 ,rd ,rs))]
    [`(xor ,rd ,rs)      `((u8 #x33) (modrm #xC0 ,rd ,rs))]
    [`(cmp ,rd ,rs)      `((u8 #x3B) (modrm #xC0 ,rd ,rs))]
    [`(not ,rd)          `((u8 #xF7) (modrm #xD0 ,rd))]
    [`(neg ,rd)          `((u8 #xF7) (modrm #xD8 ,rd))]
    [`(mul ,rd)          `((u8 #xF7) (modrm #xE0 ,rd))]
    [`(div ,rd)          `((u8 #xF7) (modrm #xF0 ,rd))]
    [`(imul ,rd)         `((u8 #xF7) (modrm #xE8 ,rd))]
    [`(idiv ,rd)         `((u8 #xF7) (modrm #xF8 ,rd))]
    [`(shl ,rd)    `((u8 #xD1 (+ #xE0 ,(asm/regix rd))))]
    [`(shr ,rd)    `((u8 #xD1 (+ #xE8 ,(asm/regix rd))))]
    [`(sar ,rd)    `((u8 #xD1 (+ #xF8 ,(asm/regix rd))))]
    [`(shl ,rd ,n) `((u8 #xC1 (+ #xE0 ,(asm/regix rd)) ,n))]
    [`(shr ,rd ,n) `((u8 #xC1 (+ #xE8 ,(asm/regix rd)) ,n))]
    [`(sar ,rd ,n) `((u8 #xC1 (+ #xF8 ,(asm/regix rd)) ,n))]
    [`(shl.cl ,rd) `((u8 #xD3 (+ #xE0 ,(asm/regix rd))))]
    [`(shr.cl ,rd) `((u8 #xD3 (+ #xE8 ,(asm/regix rd))))]
    [`(sar.cl ,rd) `((u8 #xD3 (+ #xEF ,(asm/regix rd))))]
    ;; New AVX instructions
    [`(vmovaps ,dst ,src) `((u8 #xC5 #xFC #x28) (modrm #xC0 ,dst ,src))]
    [`(vaddps ,dst ,src1 ,src2) `((u8 #xC5 #xF4 #x58) (modrm #xC0 ,dst ,src2))]
    [`(vxorps ,dst ,src1 ,src2) `((u8 #xC5 #xF8 #x57) (modrm #xC0 ,dst ,src2))]
    [`(vfmadd132ps ,dst ,src1 ,src2) `((u8 #xC4 #xE2 #x7D #x98) (modrm #xC0 ,dst ,src2))]
    ;; System calls
    [`(syscall) `((u8 #x0F #x05))]
    ;; Error
    [err (error "Invalid Instruction" err)]))

;; ================
;; Top-Level Driver
;; ================

(define *code*
  '((asm init
         (mov.imm32 rdi buffer1)
         (mov.imm32 rsi buffer2)
         (mov.imm32 rdx result)
         (vmovaps ymm0 (rdi))
         (vmovaps ymm1 (rsi))
         (vaddps ymm2 ymm0 ymm1)
         (vmovaps ymm3 (multiplier))
         (vfmadd132ps ymm2 ymm2 ymm3)
         (vmovaps (rdx) ymm2)
         (vxorps ymm2 ymm2 ymm2)
         (vmovaps (rdx) ymm2)
         (mov.imm32 eax 60)
         (xor edi edi)
         (syscall))
    (asm buffer1
         (value 32 (str "12345678123456781234567812345678")))
    (asm buffer2
         (value 32 (str "87654321876543218765432187654321")))
    (asm result
         (value 32 (repeat 32 0)))
    (asm multiplier
         (value 32 (repeat 8 #vu8(64 0 0 0))))))

(hexdump (bin/resolve (link (decls *code*))))
