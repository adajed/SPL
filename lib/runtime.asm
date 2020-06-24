default rel

global printInt
global allocMemory
global freeMemory
global print

extern free                                             ; near
extern malloc                                           ; near
extern __printf_chk                                     ; near
extern _GLOBAL_OFFSET_TABLE_                            ; byte


SECTION .text   align=16 execute                        ; section number 1, code

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
    mov rcx, rdi
    mov rax, 1
    mov rdi, 1
    lea rsi, [rcx+8]
    mov edx, [rcx+4]
    syscall
    ret

SECTION .data   align=1 noexecute                       ; section number 2, data


SECTION .bss    align=1 noexecute                       ; section number 3, bss


SECTION .rodata.str1.1 align=1 noexecute                ; section number 4, const

LC0:                                                   ; byte
        db 25H, 64H, 0AH, 00H                           ; 0000 _ %d..


