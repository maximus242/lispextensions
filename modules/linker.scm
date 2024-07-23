(define-module (linker)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (asm)
  #:export (link linker-link link/flatten link/elf-wrapper))

(define (concat xs) (apply append xs))
(define (catmap f xs) (concat (map f xs)))
(define (intersperse y xs)
  (cdr (apply append (map (lambda (x) (list y x)) xs))))
(define (symcat . xs)
  (string->symbol
    (string-concatenate (intersperse ":" (map symbol->string xs)))))

(define (resolve-symbolic-addresses instructions symbol-table)
  (map (lambda (instr)
         (match instr
           ;; Resolve symbolic addresses
           [(symbolic sym)
            (let ((resolved (hash-ref symbol-table sym #f)))
              (if resolved
                  resolved
                  (error "Unresolved symbolic address" sym)))]
           ;; Handle const instruction without resolving
           [`(const ,name ,value) instr]
           ;; Handle value instruction
           [(list 'value n x) instr]
           ;; Handle other instructions unchanged
           [else instr]))
       instructions))

(define (linker-link objs)
  (let* ((flattened (catmap link/flatten (link/elf-wrapper objs)))
         (symbols (map (lambda (obj)
                         (match obj
                           [(object ,name ,type . ,body)
                            (cons name (length body))]
                           [(const ,name $) (cons name 0)]
                           [else #f]))
                       flattened))
         (filtered-symbols (filter (lambda (x) x) symbols))
         (symbol-table (make-hash-table)))
    
    ;; Populate symbol table
    (for-each (lambda (sym)
                (hash-set! symbol-table (car sym) (cdr sym)))
              filtered-symbols)
    
    ;; Resolve symbolic addresses
    (map (lambda (obj)
           (match obj
             [(object ,name ,type . ,body)
              (let ((resolved-body (resolve-symbolic-addresses body symbol-table)))
                (object ,name ,type . ,resolved-body))]
             ;; Directly include const types as they don't need resolving
             [(const ,name $) obj]
             [else obj]))
         flattened)))

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
