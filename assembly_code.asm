; assembly_code.asm
section .text
global init
global vxorps
global vaddps
global vfmadd132ps
global vmovaps
global vbroadcastf128

init:
    ; Initialization code here (if any)
    ret

vxorps:
    vxorps ymm0, ymm0, ymm0
    ret

vaddps:
    vaddps ymm0, ymm1, ymm2
    ret

vfmadd132ps:
    vfmadd132ps ymm0, ymm1, ymm2
    ret

vmovaps:
    vmovaps ymm0, [rsp]
    ret

vbroadcastf128:
    vbroadcastf128 ymm0, [rsp]
    ret
