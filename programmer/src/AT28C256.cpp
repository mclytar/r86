#include <Arduino.h>

#include "AT28C256.h"

#define OUTPUT_ENABLE()     (PORTG &= ~0x04)
#define OUTPUT_DISABLE()    (PORTG |= 0x04)
#define WRITE_ENABLE()      (PORTG &= ~0x02)
#define WRITE_DISABLE()     (PORTG |= 0x02)
#define CHIP_ENABLE()       (PORTG &= ~0x01)
#define CHIP_DISABLE()      (PORTG |= 0x01)

#define SET_ADDRESS(VAR)    { PORTA = (VAR) & 0xFF; PORTC = ((VAR) >> 8) & 0xFF; }
#define GET_ADDRESS()       (((u16)PORTC << 8) | (u16)PORTA)
#define SET_DATA(VAR)       (PORTL = VAR)
#define GET_DATA()          (PINL)
#define SET_MODE_READ()     { PORTL = 0x00; DDRL = 0x00; }
#define SET_MODE_WRITE()    (DDRL = 0xFF)

#define WAIT_CHIP_READY()   asm("nop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\tnop\n\t")

#define STORE(ADDR, DATA)   SET_ADDRESS((ADDR)); \
    WAIT_CHIP_READY(); \
    WRITE_ENABLE(); \
    WAIT_CHIP_READY(); \
    SET_DATA((DATA)); \
    WAIT_CHIP_READY(); \
    WRITE_DISABLE(); \
    WAIT_CHIP_READY()

AT28C256::AT28C256() {
    WRITE_DISABLE();
    DDRG |= 0x07;
    OUTPUT_DISABLE();
    CHIP_DISABLE();

    DDRA = 0xFF;
    DDRC = 0xFF;
}

void AT28C256::_poll() {
    bool polling = true;
    while (polling) {
        // Poll first value.
        OUTPUT_ENABLE();
        WAIT_CHIP_READY();

        u8 val1 = GET_DATA();

        OUTPUT_DISABLE();
        WAIT_CHIP_READY();

        // Poll second value.
        OUTPUT_ENABLE();
        WAIT_CHIP_READY();

        u8 val2 = GET_DATA();

        OUTPUT_DISABLE();
        WAIT_CHIP_READY();

        // Check if the two values are the same and, in that case, stop polling.
        if (val1 == val2) polling = false;
    }
}

u16 AT28C256::read(u8 * buffer, u16 length) {
    // Data bus already in input mode.
    // SET_MODE_READ();

    // Set #OE = LOW (output enable).
    OUTPUT_ENABLE();
    WAIT_CHIP_READY();
    
    u16 i;
    for (i = 0; i < length; i++) {
        // Load information from data bus.
        buffer[i] = GET_DATA();

        // Increase address.
        u16 _addr = GET_ADDRESS();
        _addr += 1;
        if (_addr < 0x7FFF) SET_ADDRESS(_addr)
        else break;
        WAIT_CHIP_READY();
    }

    // Set #OE = HIGH (output disable).
    OUTPUT_DISABLE();
    WAIT_CHIP_READY();

    // Returns the number of bytes read.
    return i;
}

u16 AT28C256::read_byte(u8 * value) {
    // Data bus already in input mode.
    // SET_MODE_READ();

    // Set #OE = LOW (output enable).
    OUTPUT_ENABLE();
    WAIT_CHIP_READY();

    // Load information from data bus.
    *value = GET_DATA();
    
    // Set #OE = HIGH (output disable).
    OUTPUT_DISABLE();
    WAIT_CHIP_READY();

    // Increase address.
    u16 _addr = GET_ADDRESS();
    _addr += 1;
    SET_ADDRESS(_addr);
    WAIT_CHIP_READY();

    // read_byte() reads 1 byte.
    return 1;
}

u16 AT28C256::write_byte(u8 * value) {
    // Set data bus in output mode.
    SET_MODE_WRITE();

    // Set #WE = LOW (address latch).
    WRITE_ENABLE();
    WAIT_CHIP_READY();

    // Store information in data bus.
    SET_DATA(*value);
    WAIT_CHIP_READY();

    // Set #WE = HIGH (data latch).
    WRITE_DISABLE();
    WAIT_CHIP_READY();

    // Set data bus in input mode.
    SET_MODE_READ();

    // Write cycle ended; poll to learn when the data is ready.
    bool polling = true;
    while (polling) {
        // Poll first value.
        OUTPUT_ENABLE();
        WAIT_CHIP_READY();

        u8 val1 = GET_DATA();

        OUTPUT_DISABLE();
        WAIT_CHIP_READY();

        // Poll second value.
        OUTPUT_ENABLE();
        WAIT_CHIP_READY();

        u8 val2 = GET_DATA();

        OUTPUT_DISABLE();
        WAIT_CHIP_READY();

        // Check if the two values are the same and, in that case, stop polling.
        if (val1 == val2) polling = false;
    }

    // Increase address.
    u16 _addr = GET_ADDRESS();
    _addr += 1;
    SET_ADDRESS(_addr);
    WAIT_CHIP_READY();

    // write_byte() writes 1 byte.
    return 1;
}

u16 AT28C256::write_page(u8 * buffer, u8 size, u16 page) {
    // Preserve address.
    u16 _addr = GET_ADDRESS();

    // Set data bus in output mode.
    SET_MODE_WRITE();

    u8 i;
    for (i = 0; i < size; i++) {
        // Set byte of page.
        u16 _direct = ((page << 6) | (u16)i) & 0x7FFF;
        SET_ADDRESS(_direct);
        WAIT_CHIP_READY();

        // Set #WE = LOW (address latch).
        WRITE_ENABLE();
        WAIT_CHIP_READY();

        // Store information in data bus.
        SET_DATA(buffer[i]);
        WAIT_CHIP_READY();

        // Set #WE = HIGH (data latch).
        WRITE_DISABLE();
        WAIT_CHIP_READY();
    }

    // Set data bus in input mode.
    SET_MODE_READ();

    // Write cycle ended; poll to learn when the data is ready.
    this->_poll();

    // Preserve address.
    SET_ADDRESS(_addr);
    WAIT_CHIP_READY();

    // Returns the number of bytes written.
    return 1;
}

u16 AT28C256::protected_write_byte(u8 * value) {
    // Preserve address for later.
    u16 _addr = GET_ADDRESS();

    // Set data bus in output mode.
    SET_MODE_WRITE();

    // Store AA to address 5555
    STORE(0x5555, 0xAA);

    // Store 55 to address 2AAA
    STORE(0x2AAA, 0x55);

    // Store A0 to address 5555
    STORE(0x5555, 0xA0);

    // Reload preserved address.
    SET_ADDRESS(_addr);
    WAIT_CHIP_READY();

    // Set #WE = LOW (address latch).
    WRITE_ENABLE();
    WAIT_CHIP_READY();

    // Store information in data bus.
    SET_DATA(*value);
    WAIT_CHIP_READY();

    // Set #WE = HIGH (data latch).
    WRITE_DISABLE();
    WAIT_CHIP_READY();

    // Set data bus in input mode.
    SET_MODE_READ();

    // Write cycle ended; poll to learn when the data is ready.
    this->_poll();

    // Increase address.
    _addr += 1;
    SET_ADDRESS(_addr);
    WAIT_CHIP_READY();

    // write_byte() writes 1 byte.
    return 1;
}

u16 AT28C256::protected_write_page(u8 * buffer, u8 size, u16 page) {
    // Preserve address.
    u16 _addr = GET_ADDRESS();

    // Set data bus in output mode.
    SET_MODE_WRITE();

    // Store AA to address 5555
    STORE(0x5555, 0xAA);

    // Store 55 to address 2AAA
    STORE(0x2AAA, 0x55);

    // Store A0 to address 5555
    STORE(0x5555, 0xA0);

    u8 i;
    for (i = 0; i < size; i++) {
        // Set byte of page.
        u16 _direct = ((page << 6) | (u16)i) & 0x7FFF;
        SET_ADDRESS(_direct);
        WAIT_CHIP_READY();

        // Set #WE = LOW (address latch).
        WRITE_ENABLE();
        WAIT_CHIP_READY();

        // Store information in data bus.
        SET_DATA(buffer[i]);
        WAIT_CHIP_READY();

        // Set #WE = HIGH (data latch).
        WRITE_DISABLE();
        WAIT_CHIP_READY();
    }

    // Set data bus in input mode.
    SET_MODE_READ();

    // Write cycle ended; poll to learn when the data is ready.
    this->_poll();

    // Preserve address.
    SET_ADDRESS(_addr);
    WAIT_CHIP_READY();

    // Returns the number of bytes written.
    return 1;
}

u16 AT28C256::unprotect_and_write_byte(u8 * value) {
    // Preserve address for later.
    u16 _addr = GET_ADDRESS();

    // Set data bus in output mode.
    SET_MODE_WRITE();

    // Store AA to address 5555
    STORE(0x5555, 0xAA);

    // Store 55 to address 2AAA
    STORE(0x2AAA, 0x55);

    // Store 80 to address 5555
    STORE(0x5555, 0x80);

    // Store AA to address 5555
    STORE(0x5555, 0xAA);

    // Store 55 to address 2AAA
    STORE(0x2AAA, 0x55);

    // Store 20 to address 5555
    STORE(0x5555, 0x20);

    // Reload preserved address.
    SET_ADDRESS(_addr);
    WAIT_CHIP_READY();

    // Set #WE = LOW (address latch).
    WRITE_ENABLE();
    WAIT_CHIP_READY();

    // Store information in data bus.
    SET_DATA(*value);
    WAIT_CHIP_READY();

    // Set #WE = HIGH (data latch).
    WRITE_DISABLE();
    WAIT_CHIP_READY();

    // Set data bus in input mode.
    SET_MODE_READ();

    // Write cycle ended; poll to learn when the data is ready.
    this->_poll();

    // Increase address.
    _addr += 1;
    SET_ADDRESS(_addr);
    WAIT_CHIP_READY();

    // write_byte() writes 1 byte.
    return 1;
}

u16 AT28C256::unprotect_and_write_page(u8 * buffer, u8 size, u16 page) {
    // Preserve address.
    u16 _addr = GET_ADDRESS();

    // Set data bus in output mode.
    SET_MODE_WRITE();

    // Store AA to address 5555
    STORE(0x5555, 0xAA);

    // Store 55 to address 2AAA
    STORE(0x2AAA, 0x55);

    // Store 80 to address 5555
    STORE(0x5555, 0x80);

    // Store AA to address 5555
    STORE(0x5555, 0xAA);

    // Store 55 to address 2AAA
    STORE(0x2AAA, 0x55);

    // Store 20 to address 5555
    STORE(0x5555, 0x20);

    u8 i;
    for (i = 0; i < size; i++) {
        // Set byte of page.
        u16 _direct = ((page << 6) | (u16)i) & 0x7FFF;
        SET_ADDRESS(_direct);
        WAIT_CHIP_READY();

        // Set #WE = LOW (address latch).
        WRITE_ENABLE();
        WAIT_CHIP_READY();

        // Store information in data bus.
        SET_DATA(buffer[i]);
        WAIT_CHIP_READY();

        // Set #WE = HIGH (data latch).
        WRITE_DISABLE();
        WAIT_CHIP_READY();
    }

    // Set data bus in input mode.
    SET_MODE_READ();

    // Write cycle ended; poll to learn when the data is ready.
    this->_poll();

    // Preserve address.
    SET_ADDRESS(_addr);
    WAIT_CHIP_READY();

    // Returns the number of bytes written.
    return 1;
}

void AT28C256::enable() {
    CHIP_ENABLE();
}

void AT28C256::disable() {
    CHIP_DISABLE();
}

void AT28C256::seek(u16 address) {
    SET_ADDRESS(address);
}

u16 AT28C256::pos() const {
    return GET_ADDRESS();
}