;; assembly_code.asm
section .text
global vxorps
vxorps:
    vxorps ymm0, ymm0, ymm0
    ret

global vaddps
vaddps:
    vaddps ymm0, ymm1, ymm2
    ret

global vfmadd132ps
vfmadd132ps:
    vfmadd132ps ymm0, ymm1, ymm2
    ret

global vmovaps
vmovaps:
    vmovaps ymm0, [rsp]
    ret

global vbroadcastf128
vbroadcastf128:
    vbroadcastf128 ymm0, [rsp]
    ret
