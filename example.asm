_start:
                                cli

                                ; RAM test
                                xor     ax, ax
                                mov     ds, ax
                                mov     es, ax
                                mov     si, ax
                                mov     di, ax
                                mov     ax, 0x5555
                                mov     bx, 0xAAAA
    ram_test:
                                xor     cx, cx
                                dec     cx
                                shr     cx, 1
                                rep     stosw
                                xor     cx, cx
                                dec     cx
                                shr     cx, 1
                                mov     dx, ax
        .check_5:               lodsw
                                test    dx, ax
                                jnz     .error
                                loop    .check_5
                                mov     ax, bx
                                xor     cx, cx
                                dec     cx
                                shr     cx, 1
                                rep     stosw
                                xor     cx, cx
                                dec     cx
                                shr     cx, 1
                                mov     dx, ax
        .check_A:               lodsw
                                test    dx, ax
                                jnz     .error
                                loop    .check_A
                                jmp     finish
        .error:                 mov     al, 0x01
                                out     0xFE, al
                                hlt
    finish:
                                mov     al, 0x02
                                out     0xFE, al
                                hlt

TIMES   65520 - ($ - $$)        nop