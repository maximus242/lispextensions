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
                   ;; Add other addressing modes if needed
                   [else (error "Invalid mod value" mod)]))
        (reg-val (asm/regix reg))
        (rm-val (case rm
                  [(rdi) 5]  ;; Memory addressing via rdi register with 32-bit displacement
                  ;; Add other memory registers as needed
                  [else (asm/regix rm)])))
    (if (and (not (eq? reg-val #f)) (not (eq? rm-val #f)))
        (begin
          (display (format #f "modrm intermediate values - mod-val: ~a, reg-val: ~a, rm-val: ~a\n" mod-val reg-val rm-val))
          (let ((result (+ mod-val (* 8 reg-val) rm-val))) ;; Ensure correct bit shifting
            (display (format #f "modrm result: ~a\n" result))
            result))
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

(define (u32 byte1 byte2 byte3 byte4)
  "Convert four byte values to their string representation."
  `((byte ,byte1) (byte ,byte2) (byte ,byte3) (byte ,byte4)))

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
    [`(u32 ,byte1 ,byte2 ,byte3 ,byte4) (begin
                                          (display (format #f "Desugaring u32 bytes: ~a, ~a, ~a, ~a\n" byte1 byte2 byte3 byte4))
                                          `((byte ,byte1) (byte ,byte2) (byte ,byte3) (byte ,byte4)))]
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
    ;; Handle vmovaps instruction
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
           ;; Generate the correct bytecode for mov.imm32
           (list (u8 (+ #xB8 rd-ix)) (u32 imm))
           (error "Invalid mov.imm32 register" rd)))]
    ;; Handle other instructions
    [err (error "Invalid Instruction" err)]))
