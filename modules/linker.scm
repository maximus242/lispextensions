;; modules/linker.scm
(define-module (linker)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (linker-link link/flatten link/elf-wrapper))

(define (concat xs) (apply append xs))
(define (catmap f xs) (concat (map f xs)))
(define (intersperse y xs)
  (cdr (apply append (map (lambda (x) (list y x)) xs))))
(define (symcat . xs)
  (string->symbol
    (string-concatenate (intersperse ":" (map symbol->string xs)))))

(define (resolve-symbolic-addresses instructions symbol-table)
  (display "Entering resolve-symbolic-addresses\n")
  (map (lambda (instr)
         (display (format #f "Resolving instruction: ~a\n" instr))
         (match instr
           ;; Resolve symbolic addresses
           [(symbolic sym)
            (let ((resolved (hash-ref symbol-table sym #f)))
              (if resolved
                  resolved
                  (error "Unresolved symbolic address" sym)))]
           ;; Handle const instruction without resolving
           [`(const ,name ,value)
            (display (format #f "Handling const: ~a\n" instr))
            `(const ,name ,value)]
           ;; Handle value instruction
           [(list 'value n x)
            (display (format #f "Handling value: ~a\n" instr))
            instr]
           ;; Handle other instructions unchanged
           [else
            (display (format #f "Handling other: ~a\n" instr))
            instr]))
       instructions))

(define (linker-link objs)
  (display "Entering linker-link\n")
  
  ;; Print the input objects
  (display (format #f "Input objects: ~a\n" objs))
  
  (let* ((flattened (catmap link/flatten objs))
         (symbols (map (lambda (obj)
                         (match obj
                           ;; Handle the object type
                           [(object ,name ,type . ,body)
                            (display (format #f "Processing object: ~a\n" obj))
                            (cons name (length body))]
                           ;; Handle the const type
                           [(const ,name $)
                            (display (format #f "Processing const: ~a\n" obj))
                            (cons name 0)]
                           [else
                            (display (format #f "Unexpected object structure: ~a\n" obj))
                            #f]))
                       flattened))
         (filtered-symbols (filter (lambda (x) x) symbols))
         (symbol-table (make-hash-table)))
    
    ;; Populate symbol table
    (display (format #f "Symbols: ~a\n" filtered-symbols))
    (for-each (lambda (sym)
                (display (format #f "Adding to symbol table: ~a\n" sym))
                (hash-set! symbol-table (car sym) (cdr sym)))
              filtered-symbols)
    
    ;; Resolve symbolic addresses
    (let ((resolved (map (lambda (obj)
                           (match obj
                             [(object ,name ,type . ,body)
                              (display (format #f "Resolving object: ~a\n" obj))
                              (let ((resolved-body (resolve-symbolic-addresses body symbol-table)))
                                (display (format #f "Resolved body: ~a\n" resolved-body))
                                (object ,name ,type . ,resolved-body))]
                             ;; Directly include const types as they don't need resolving
                             [(const ,name $)
                              (display (format #f "Including const without resolving: ~a\n" obj))
                              obj]
                             [(modrm ,mod ,reg ,rm)
                              (display (format #f "Handling modrm: ~a\n" obj))
                              obj]
                             [(byte ,b)
                              (display (format #f "Handling byte: ~a\n" obj))
                              obj]
                             [else
                              (display (format #f "Unexpected object structure during resolve: ~a\n" obj))
                              obj]))
                         flattened)))
      (display (format #f "Resolved objects: ~a\n" resolved))
      resolved)))

(define (link/flatten obj)
  (match obj
    [`(object ,name ,type . ,body)
     (display (format #f "Flattening object: ~a\n" obj))
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
