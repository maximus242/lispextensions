;;;; Modified from http://www.willdonnelly.net/blog/2021-05-06-scheme-x86-assembler/ to work with Guile.

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
;;
;; At the bottom of the stack of translations is a
;; tiny language for describing binary files. This
;; language has the following forms:
;;
;;   (ORIGIN 123)   Set the current address to 123
;;   (ALIGN 16)     Zero pad to a multiple of 16
;;   (CONST n x)    Evaluate X and define constant N
;;   (VALUE k x)    Evaluate X and emit as K bytes
;;
;; The resolution proceeds in two passes. First all
;; CONST statements are evaluated in order to produce
;; a name-value mapping, then all VALUE statements
;; are evaluated in order to produce output bytes.

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
      (let ((len (modulo (- 0 off) size)))
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
      (let ((len (modulo (- 0 off) size)))
        (bin/consts (+ off len) xs env))]
    [`((value ,n ,x) . ,xs) (bin/consts (+ off n) xs env)]
    [err (error "Invalid Statement" err)]))

(define (bin/resolve stmts)
  (bin/values '() 0 stmts (bin/consts 0 stmts '())))

;; =============================================
;; LINK: Combining and Renaming Chunks of Binary
;; =============================================
;;
;; The linker is responsible for combining relocatable
;; "objects" into a (Linux ELF) binary. The current linker
;; is trivial and just shoves everything into a single
;; RWX segment, but could be improved to handle different
;; "types" of object (text/data/bss/rodata/etc).

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
      ;; [`((object ,@b) ,@ys) (step (cons (car xs) out) ys)]
      ;; [`(,d ,@ys) (step out (append (decls/expand d) ys))]
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
;;
;; Lowers the platform instruction set into a sequence
;; of CONST and VALUE statements by repeatedly applying
;; a desugaring transform.

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
    ['rdi 7] ['rsi 6] ['rdx 2] ['rcx 1] ['rax 0]
    ['ymm0 0] ['ymm1 1] ['ymm2 2] ['ymm3 3]
    ['ymm4 4] ['ymm5 5] ['ymm6 6] ['ymm7 7]
    [x (error "Invalid Register" reg)]))

(define (string->bytes str)
  (map char->integer (string->list str)))

(define (asm/desugar inst)
  "Decomposes a high-level instruction into a sequence of"
  "simpler instructions. Note that the resulting 'values'"
  "are still expressions whose actual numeric value may"
  "be computed in a later phase."
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
    [`(push ,rs) `((u8 (+ #x50 ,(asm/regix rs))))]
    [`(pop ,rd)  `((u8 (+ #x58 ,(asm/regix rd))))]
    [`(ret)      `((u8 #xC3))]
    [`(ret ,n)   `((u8 #xC2) (u16 ,n))]
    [`(call.reg ,rs) `((u8 #xFF (+ #xD0 ,(asm/regix rs))))]
    [`(call ,addr) `((u8 #xE8)      (disp ,addr))]
    [`(jmp ,addr)  `((u8 #xE9)      (disp ,addr))]
    [`( jb ,addr)  `((u8 #x0F #x82) (disp ,addr))]
    [`(jae ,addr)  `((u8 #x0F #x83) (disp ,addr))]
    [`( je ,addr)  `((u8 #x0F #x84) (disp ,addr))]
    [`(jne ,addr)  `((u8 #x0F #x85) (disp ,addr))]
    [`(jbe ,addr)  `((u8 #x0F #x86) (disp ,addr))]
    [`( ja ,addr)  `((u8 #x0F #x87) (disp ,addr))]
    [`( jl ,addr)  `((u8 #x0F #x8C) (disp ,addr))]
    [`(jge ,addr)  `((u8 #x0F #x8D) (disp ,addr))]
    [`(jle ,addr)  `((u8 #x0F #x8E) (disp ,addr))]
    [`( jg ,addr)  `((u8 #x0F #x8F) (disp ,addr))]
    [`(jmp.short ,addr) `((u8 #xEB) (disp.short ,addr))]
    [`( jb.short ,addr) `((u8 #x72) (disp.short ,addr))]
    [`(jae.short ,addr) `((u8 #x73) (disp.short ,addr))]
    [`( je.short ,addr) `((u8 #x74) (disp.short ,addr))]
    [`(jne.short ,addr) `((u8 #x75) (disp.short ,addr))]
    [`(jbe.short ,addr) `((u8 #x76) (disp.short ,addr))]
    [`( ja.short ,addr) `((u8 #x77) (disp.short ,addr))]
    [`( jl.short ,addr) `((u8 #x7C) (disp.short ,addr))]
    [`(jge.short ,addr) `((u8 #x7D) (disp.short ,addr))]
    [`(jle.short ,addr) `((u8 #x7E) (disp.short ,addr))]
    [`( jg.short ,addr) `((u8 #x7F) (disp.short ,addr))]
    [`(set.eq ,rd) `((u8 #x0F #x94) (modrm #xC0 ,rd))]
    [`(set.ne ,rd) `((u8 #x0F #x95) (modrm #xC0 ,rd))]
    [`(set.lt ,rd) `((u8 #x0F #x9C) (modrm #xC0 ,rd))]
    [`(set.le ,rd) `((u8 #x0F #x9E) (modrm #xC0 ,rd))]
    [`(set.gt ,rd) `((u8 #x0F #x9F) (modrm #xC0 ,rd))]
    [`(set.ge ,rd) `((u8 #x0F #x9D) (modrm #xC0 ,rd))]
    [`(int ,n)     `((u8 #xCD ,n))]
    ;; Handle vxorps instruction
    [`(vxorps ,dst ,src1 ,src2)
     (let ((dst-ix (asm/regix dst))
           (src1-ix (asm/regix src1))
           (src2-ix (asm/regix src2)))
       (display (format #f "vxorps - dst: ~a, src1: ~a, src2: ~a\n" dst src1 src2))
       (display (format #f "vxorps - dst-ix: ~a, src1-ix: ~a, src2-ix: ~a\n" dst-ix src1-ix src2-ix))
       ;; Check if the registers are valid
       (if (and dst-ix src1-ix src2-ix)
           ;; Generate the correct bytecode for vxorps
           (let ((modrm-byte (modrm #xC0 dst src1)))
             (display (format #f "vxorps - modrm byte: ~a\n" modrm-byte))
             (list (u8 #xC5) (u8 #xFC) (u8 #x57) (u8 modrm-byte)))
           (error "Invalid vxorps registers" dst src1 src2)))]
    ;; Handle vaddps instruction
    [`(vaddps ,dst ,src1 ,src2)
     (let ((dst-ix (asm/regix dst))
           (src1-ix (asm/regix src1))
           (src2-ix (asm/regix src2)))
       (display (format #f "vaddps - dst: ~a, src1: ~a, src2: ~a\n" dst src1 src2))
       (display (format #f "vaddps - dst-ix: ~a, src1-ix: ~a, src2-ix: ~a\n" dst-ix src1-ix src2-ix))
       ;; Check if the registers are valid
       (if (and dst-ix src1-ix src2-ix)
           ;; Generate the correct bytecode for vaddps
           (let ((modrm-byte (modrm #xC0 dst src2)))
             (display (format #f "vaddps - modrm byte: ~a\n" modrm-byte))
             (list (u8 #xC5) (u8 #xF4) (u8 #x58) (u8 modrm-byte)))
           (error "Invalid vaddps registers" dst src1 src2)))]
    ;; Handle vmovaps instruction with memory operand as destination
    [`(vmovaps ,(mem-op) ,src)
     (let ((src-ix (asm/regix src)))
       (display (format #f "vmovaps - mem-op: ~a, src: ~a\n" mem-op src))
       (display (format #f "vmovaps - src-ix: ~a\n" src-ix))
       ;; Check if the registers are valid
       (if src-ix
           ;; Generate the correct bytecode for vmovaps with memory operand as destination
           (let ((modrm-byte (modrm #x00 src mem-op))) ;; Memory addressing mode
             (display (format #f "vmovaps - modrm byte: ~a\n" modrm-byte))
             (list (u8 #xC5) (u8 #xFC) (u8 #x29) (u8 modrm-byte) (u8 #x00) (u8 #x00) (u8 #x00) (u8 #x00)))
           (error "Invalid vmovaps register or memory operand" src mem-op)))]
    ;; Handle vmovaps instruction with memory operand as source
    [`(vmovaps ,dst ,(mem-op))
     (let ((dst-ix (asm/regix dst)))
       (display (format #f "vmovaps - dst: ~a, mem-op: ~a\n" dst mem-op))
       (display (format #f "vmovaps - dst-ix: ~a\n" dst-ix))
       ;; Check if the registers are valid
       (if dst-ix
           ;; Generate the correct bytecode for vmovaps with memory operand
           (let ((modrm-byte (modrm #x00 dst mem-op))) ;; Memory addressing mode
             (display (format #f "vmovaps - modrm byte: ~a\n" modrm-byte))
             (list (u8 #xC5) (u8 #xFC) (u8 #x28) (u8 modrm-byte) (u8 #x00) (u8 #x00) (u8 #x00) (u8 #x00)))
           (error "Invalid vmovaps register or memory operand" dst mem-op)))]
    ;; Handle vbroadcastf128 instruction
    [`(vbroadcastf128 ,dst ,mem-op)
     (let ((dst-ix (asm/regix dst)))
       (display (format #f "vbroadcastf128 - dst: ~a, mem-op: ~a\n" dst mem-op))
       (display (format #f "vbroadcastf128 - dst-ix: ~a\n" dst-ix))
       ;; Check if the registers are valid
       (if dst-ix
           ;; Generate the correct bytecode for vbroadcastf128 with memory operand
           (let ((modrm-byte (modrm #x05 dst mem-op))) ;; Memory addressing mode with displacement
             (display (format #f "vbroadcastf128 - modrm byte: ~a\n" modrm-byte))
             ;; Include displacement bytes (4 bytes of zero in this case)
             (list (u8 #xC4) (u8 #xE2) (u8 #x7D) (u8 #x1A) (u8 modrm-byte) (u8 #x00) (u8 #x00) (u8 #x00) (u8 #x00)))
           (error "Invalid vbroadcastf128 register or memory operand" dst mem-op)))]
    ;; Handle vfmadd132ps instruction
    [`(vfmadd132ps ,dst ,src1 ,src2)
     (let ((dst-ix (asm/regix dst))
           (src1-ix (asm/regix src1))
           (src2-ix (asm/regix src2)))
       (display (format #f "vfmadd132ps - dst: ~a, src1: ~a, src2: ~a\n" dst src1 src2))
       (display (format #f "vfmadd132ps - dst-ix: ~a, src1-ix: ~a, src2-ix: ~a\n" dst-ix src1-ix src2-ix))
       (if (and dst-ix src1-ix src2-ix)
           ;; Ensure the correct byte order for VEX prefix, opcode, and ModR/M byte
           (let ((modrm-byte (modrm #xC0 dst src2)))
             (display (format #f "vfmadd132ps - modrm byte: ~a\n" modrm-byte))
             (list (u8 #xC4) (u8 #xE2) (u8 #x75) (u8 #x98) (u8 modrm-byte)))
           (error "Invalid vfmadd132ps registers" dst src1 src2)))]
    ;; Handle mov.imm32 instruction
    [`(mov.imm32 ,rd ,imm)
     (let ((rd-ix (asm/regix rd)))
       (display (format #f "mov.imm32 - rd: ~a, imm: ~a\n" rd imm))
       (if rd-ix
           (list (u8 (+ #xB8 rd-ix)) (u32 imm))
           (error "Invalid mov.imm32 register" rd)))]
    ;; Handle syscall instruction
    [`(syscall)
     (list (u8 #x0F) (u8 #x05))]
    ;; Handle xor instruction
    [`(xor ,dst ,src)
     (let ((dst-ix (asm/regix dst))
           (src-ix (asm/regix src)))
       (if (and dst-ix src-ix)
           (let ((modrm-byte (modrm #xC0 dst src)))
             (list (u8 #x33) (u8 modrm-byte)))
           (error "Invalid xor registers" dst src)))]

    ;; Error
    [err (error "Invalid Instruction" err)]))

;; ================
;; Top-Level Driver
;; ================

(define *code*
  '((asm init
         ;; Load addresses of buffers into registers
         (mov.imm32 rdi buffer1)
         (mov.imm32 rsi buffer2)
         (mov.imm32 rdx result)
         ;; Load data from buffers into ymm registers
         (vmovaps ymm0 (rdi))
         (vmovaps ymm1 (rsi))
         ;; Add the two buffers
         (vaddps ymm2 ymm0 ymm1)
         ;; Multiply the result by a constant (e.g., 2.0)
         (vmovaps ymm3 (multiplier))
         (vfmadd132ps ymm2 ymm2 ymm3)
         ;; Store the result back into the result buffer
         (vmovaps (rdx) ymm2)
         ;; Zero out the result buffer
         (vxorps ymm2 ymm2 ymm2)
         (vmovaps (rdx) ymm2)
         ;; Exit
         (mov.imm32 eax 60)  ;; syscall: exit
         (xor edi edi)       ;; status: 0
         (syscall))

    (asm buffer1
         (value 32 (str "12345678123456781234567812345678")))
    (asm buffer2
         (value 32 (str "87654321876543218765432187654321")))
    (asm result
         (value 32 (repeat 32 0)))
    (asm multiplier
         (value 32 (repeat 8 #vu8(64 0 0 0))) ;; 2.0 in IEEE 754 floating-point
    )))

(hexdump (bin/resolve (link (decls *code*))))
