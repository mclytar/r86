section         .text
value_1:                        db      0x20
value_2:                        dw      0x1234

                                mov     ax, cx
                                mov     ah, al
                                mov     [0x1234], bx
                                mov     [value_1], bl
                                mov     ax, [bx + si]
                                mov     [bx + si + $ - $$], cx
                                mov     si, 0x7C00
                                mov     di, value_2 - value_1
                                mov     di, value_4 - value_3
                                mov     al, value_2 - value_1
                                mov     al, value_4 - value_3
                                mov     [bx], BYTE 0
                                mov     WORD [bx], 420
                                mov     ax, [si]
                                mov     [di], al
                                mov     es, dx
                                mov     es, [bp]
                                mov     ax, es
                                mov     [bp + di], es


value_3:                        db      0x20
value_4:                        dw      0x1234