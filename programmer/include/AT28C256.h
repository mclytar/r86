#ifndef __AT28C256_H_
#define __AT28C256_H_

#include <stdint.h>

class AT28C256 {
    void _poll();
public:
    AT28C256();

    u16 read(u8 *, u16);
    u16 read_byte(u8 *);

    u16 write_byte(u8 *);
    u16 write_page(u8 *, u8, u16);

    u16 protected_write_byte(u8 *);
    u16 protected_write_page(u8 *, u8, u16);

    u16 unprotect_and_write_byte(u8 *);
    u16 unprotect_and_write_page(u8 *, u8, u16);

    void enable();
    void disable();

    void seek(u16);
    u16 pos() const;
};


#endif