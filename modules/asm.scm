(define-module (asm)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:export (decls decls/expand modrm asm asm/regix string->bytes asm/desugar u8 u16 u32 u32-symbolic))

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
                   [(#x00) 0]   ;; Memory addressing mode without displacement
                   [(#x02) 64]  ;; Memory addressing mode with 8-bit displacement
                   [(#x05) 0]   ;; Memory addressing mode with 32-bit displacement
                   [else (error "Invalid mod value" mod)]))
        (reg-val (asm/regix reg))
        (rm-val (if (symbol? rm)
                    5 ;; Placeholder for memory operand
                    (asm/regix rm))))
    (if (and reg-val rm-val)
        (let ((result (+ mod-val (* 8 reg-val) rm-val)))
          (display (format #f "modrm result: ~a\n" result))
          result)
        (error "Invalid modrm values" mod reg rm))))

(define (asm xs)
  (display "Entering asm function\n")
  (define (step out xs)
    (match xs
      [`((const ,n ,x) . ,ys)
       (display (format #f "Adding const: ~a ~a\n" n x))
       (step (append out `((const ,n ,x))) ys)]
      [`((value ,n ,x) . ,ys)
       (display (format #f "Adding value: ~a ~a\n" n x))
       (step (append out `((value ,n ,x))) ys)]
      [`(,inst . ,ys)
       (begin
         (display (format #f "Desugaring instruction: ~a\n" inst))
         (let ((desugared (asm/desugar inst)))
           (display (format #f "Desugared to: ~a\n" desugared))
           (step (append out desugared) ys)))]
      [_ out]))
  (let ((result (step '() xs)))
    (display (format #f "Final result: ~a\n" result))
    (display "Exiting asm function\n")
    result))

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

(define (u8 byte)
  "Convert a byte value to its string representation."
  `(byte ,byte))

(define (u16 byte1 byte2)
  "Convert two byte values to their string representation."
  `((byte ,byte1) (byte ,byte2)))

(define (u32 imm)
  (if (integer? imm)
      (list (u8 (modulo imm 256))
            (u8 (modulo (quotient imm 256) 256))
            (u8 (modulo (quotient imm 65536) 256))
            (u8 (quotient imm 16777216)))
      ;; Handle symbolic addresses
      (u32-symbolic 0 0 0 imm)))

(define (u32-symbolic byte1 byte2 byte3 sym)
  "Handle u32 instruction with symbolic address"
  `((byte ,byte1) (byte ,byte2) (byte ,byte3) (symbolic ,sym)))

(define (asm/desugar inst)
  "Decomposes a high-level instruction into a sequence of simpler instructions."
  (display (format #f "Desugaring instruction: ~a\n" inst))
  (match inst
    ;; Handle u8 instruction with multiple bytes
    [`(u8 . ,bytes) (begin
                      (display (format #f "Desugaring multiple u8 bytes: ~a\n" bytes))
                      (map (lambda (byte) `(byte ,byte)) bytes))]
    ;; Handle u16 instruction with two bytes
    [`(u16 ,byte1 ,byte2) (begin
                            (display (format #f "Desugaring u16 bytes: ~a, ~a\n" byte1 byte2))
                            `((byte ,byte1) (byte ,byte2)))]
    ;; Handle u32 instruction with four bytes
    [`(u32 ,imm) (begin
                   (display (format #f "Desugaring u32 with imm: ~a\n" imm))
                   (u32 imm))]
    ;; Handle u32 instruction with symbolic addresses
    [`(u32 ,byte1 ,byte2 ,byte3 ,sym)
     (begin
       (display (format #f "Desugaring u32 with symbolic address: ~a, ~a, ~a, ~a\n" byte1 byte2 byte3 sym))
       ;; Treat the symbolic address as a placeholder to be resolved later
       (u32-symbolic byte1 byte2 byte3 sym))]
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

    ;; Handle remaining instructions
    [`(vals ,n) `()]
    [`(vals ,n ,x . ,xs) `((value ,n ,x) (vals ,n . ,xs))]
    [`(u16 . ,xs) `((vals 2 . ,xs))]
    [`(u32 . ,xs) `((vals 4 . ,xs))]
    [`(u64 . ,xs) `((vals 8 . ,xs))]
    [`(str ,x) `((u8 . ,(string->bytes x)) (u8 0))]
    [`(repeat 0 ,x) `()]
    [`(repeat ,n . ,xs) `(,@xs (repeat ,(- n 1) . ,xs))]
    [`(label ,n) `((const ,n $))]
    [`(disp ,addr)       `((u32 (- ,addr (+ $ 4))))]
    [`(disp.short ,addr) `((u8 (- ,addr (+ $ 1))))]
    [`(modrm ,mod ,rm) `((u8 (+ ,mod ,(asm/regix rm))))]
    [`(modrm ,mod ,reg ,rm)
     `((u8 (+ ,mod (* ,(asm/regix reg) 8) ,(asm/regix rm))))]
    [`(nop) `((u8 #x90))]
    [`(cdq) `((u8 #x99))]
    [`(inc ,rd) `((u8 (+ #x40 ,(asm/regix rd))))]
    [`(dec ,rd) `((u8 (+ #x48 ,(asm/regix rd))))]
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
    [`(or ,rd ,rs)      `((u8 #x0B) (modrm #xC0 ,rd ,rs))]
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
    [`(jb ,addr)  `((u8 #x0F #x82) (disp ,addr))]
    [`(jae ,addr)  `((u8 #x0F #x83) (disp ,addr))]
    [`(je ,addr)  `((u8 #x0F #x84) (disp ,addr))]
    [`(jne ,addr)  `((u8 #x0F #x85) (disp ,addr))]
    [`(jbe ,addr)  `((u8 #x0F #x86) (disp ,addr))]
    [`(ja ,addr)  `((u8 #x0F #x87) (disp ,addr))]
    [`(jl ,addr)  `((u8 #x0F #x8C) (disp ,addr))]
    [`(jge ,addr)  `((u8 #x0F #x8D) (disp ,addr))]
    [`(jle ,addr)  `((u8 #x0F #x8E) (disp ,addr))]
    [`(jg ,addr)  `((u8 #x0F #x8F) (disp ,addr))]
    [`(jmp.short ,addr) `((u8 #xEB) (disp.short ,addr))]
    [`(jb.short ,addr) `((u8 #x72) (disp.short ,addr))]
    [`(jae.short ,addr) `((u8 #x73) (disp.short ,addr))]
    [`(je.short ,addr) `((u8 #x74) (disp.short ,addr))]
    [`(jne.short ,addr) `((u8 #x75) (disp.short ,addr))]
    [`(jbe.short ,addr) `((u8 #x76) (disp.short ,addr))]
    [`(ja.short ,addr) `((u8 #x77) (disp.short ,addr))]
    [`(jl.short ,addr) `((u8 #x7C) (disp.short ,addr))]
    [`(jge.short ,addr) `((u8 #x7D) (disp.short ,addr))]
    [`(jle.short ,addr) `((u8 #x7E) (disp.short ,addr))]
    [`(jg.short ,addr) `((u8 #x7F) (disp.short ,addr))]
    [`(set.eq ,rd) `((u8 #x0F #x94) (modrm #xC0 ,rd))]
    [`(set.ne ,rd) `((u8 #x0F #x95) (modrm #xC0 ,rd))]
    [`(set.lt ,rd) `((u8 #x0F #x9C) (modrm #xC0 ,rd))]
    [`(set.le ,rd) `((u8 #x0F #x9E) (modrm #xC0 ,rd))]
    [`(set.gt ,rd) `((u8 #x0F #x9F) (modrm #xC0 ,rd))]
    [`(set.ge ,rd) `((u8 #x0F #x9D) (modrm #xC0 ,rd))]
    [`(int ,n)     `((u8 #xCD ,n))]
    ;; Error
    [err (error "Invalid Instruction" err)]))
