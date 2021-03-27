main:
            nop
            add     ax, cx
            sub     al, ah
            add     WORD [es:bx + si + + (MyStruct + 2) + 8], 0x80   ; some comment
    xyz:    add     x + (y, z
            inc     ch
            label1  label2
            inc     dx      ; some other comment
    .L0:    nop
    Another_Label:
            aaa
    loop:   xor     ax, ax
            jmp     loop