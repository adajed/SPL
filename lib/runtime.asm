default rel

global printInt
global allocMemory
global freeMemory
global print

extern free                                             ; near
extern malloc                                           ; near
extern __printf_chk                                     ; near
extern _GLOBAL_OFFSET_TABLE_                            ; byte


SECTION .text   align=16

printInt:
        lea     rsi, [rel LC0]
        mov     edx, edi
        xor     eax, eax
        mov     edi, 1
        jmp     __printf_chk


ALIGN   16

allocMemory:
        movsxd  rdi, edi                                ; 0020 _ 48: 63. FF
        jmp     malloc                                  ; 0023 _ E9, 00000000(PLT r)

ALIGN   16

freeMemory:
        jmp     free                                    ; 0030 _ E9, 00000000(PLT r)

ALIGN   16

print:
    push rbx
    mov rbx, rdi
    mov rax, 1
    mov rdi, 1
    lea rsi, [rbx+8]
    mov edx, [rbx+4]
    syscall
    dec DWORD [rbx]
    cmp DWORD [rbx], DWORD 0
    jne .L1
    mov rdi, rbx
    call free
.L1:
    pop rbx
    ret

SECTION .data   align=1


SECTION .bss    align=1


SECTION .rodata.str1.1 align=1

LC0:                                                   ; byte
        db 25H, 64H, 0AH, 00H                           ; 0000 _ %d..


