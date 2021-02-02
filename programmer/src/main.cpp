#include <Arduino.h>

#include "AT28C256.h"
#include "CLI.h"

#define PIN_LED_OE      10
#define PIN_LED_CE      11
#define PIN_LED_WE      12

AT28C256 eeprom;
Interface interface;

u8 chip_enable() {
    eeprom.enable();
    digitalWrite(PIN_LED_CE, HIGH);

    return ERROR_OK;
}

u8 chip_disable() {
    eeprom.disable();
    digitalWrite(PIN_LED_CE, LOW);

    return ERROR_OK;
}

u8 query_address(u16 * address) {
    *address = eeprom.pos();

    return ERROR_OK;
}

u8 select_address(u16 * address) {
    if (*address >= 32768) return ERROR_OUT_OF_RANGE;
    
    eeprom.seek(*address);
    *address = eeprom.pos();

    return ERROR_OK;
}

u8 ping(u16 * data) {
    *data = 0x55AA;

    return ERROR_OK;
}

u8 read_begin() {
    digitalWrite(PIN_LED_OE, HIGH);

    return ERROR_OK;
}

u8 read_end() {
    digitalWrite(PIN_LED_OE, LOW);

    return ERROR_OK;
}

u8 write_begin() {
    digitalWrite(PIN_LED_WE, HIGH);

    return ERROR_OK;
}

u8 write_end() {
    digitalWrite(PIN_LED_WE, LOW);

    return ERROR_OK;
}

u8 read_byte(u16 * address, u8 * data) {
    if (address) {
        if (*address >= 32768) return ERROR_OUT_OF_RANGE;

        u16 old_address = eeprom.pos();
        eeprom.seek(*address);
        eeprom.read_byte(data);
        eeprom.seek(old_address);
    } else {
        u16 amount = eeprom.read_byte(data);
        if (amount == 0) return ERROR_END_OF_FILE;
    }
    
    return ERROR_OK;
}

u8 read_page(u16 page, u8 * data) {
    if (page >= 512) return ERROR_OUT_OF_RANGE;

    u16 old_address = eeprom.pos();
    eeprom.seek(page << 6);
    eeprom.read(data, 64);
    eeprom.seek(old_address);

    return ERROR_OK;
}

u8 write_byte(u16 * address, u8 * data) {
    if (address) {
        if (*address >= 32768) return ERROR_OUT_OF_RANGE;

        u16 old_address = eeprom.pos();
        eeprom.seek(*address);
        eeprom.write_byte(data);
        eeprom.seek(old_address);
    } else {
        u16 amount = eeprom.write_byte(data);
        if (amount == 0) return ERROR_END_OF_FILE;
    }
    
    return ERROR_OK;
}

u8 protected_write_byte(u16 * address, u8 * data) {
    if (address) {
        if (*address >= 32768) return ERROR_OUT_OF_RANGE;

        u16 old_address = eeprom.pos();
        eeprom.seek(*address);
        eeprom.protected_write_byte(data);
        eeprom.seek(old_address);
    } else {
        u16 amount = eeprom.protected_write_byte(data);
        if (amount == 0) return ERROR_END_OF_FILE;
    }
    
    return ERROR_OK;
}

u8 unprotect_and_write_byte(u16 * address, u8 * data) {
    if (address) {
        if (*address >= 32768) return ERROR_OUT_OF_RANGE;

        u16 old_address = eeprom.pos();
        eeprom.seek(*address);
        eeprom.unprotect_and_write_byte(data);
        eeprom.seek(old_address);
    } else {
        u16 amount = eeprom.unprotect_and_write_byte(data);
        if (amount == 0) return ERROR_END_OF_FILE;
    }
    
    return ERROR_OK;
}

u8 write_page(u16 page, u8 * data) {
    if (page >= 512) return ERROR_OUT_OF_RANGE;

    eeprom.write_page(data, 64, page);
    
    return ERROR_OK;
}

u8 protected_write_page(u16 page, u8 * data) {
    if (page >= 512) return ERROR_OUT_OF_RANGE;

    eeprom.protected_write_page(data, 64, page);
    
    return ERROR_OK;
}

u8 unprotect_and_write_page(u16 page, u8 * data) {
    if (page >= 512) return ERROR_OUT_OF_RANGE;

    eeprom.unprotect_and_write_page(data, 64, page);
    
    return ERROR_OK;
}

void setup() {
    Serial.begin(38400);

    pinMode(PIN_LED_OE, OUTPUT);
    pinMode(PIN_LED_CE, OUTPUT);
    pinMode(PIN_LED_WE, OUTPUT);

    interface.on_chip_enable(chip_enable);
    interface.on_chip_disable(chip_disable);
    interface.on_query_address(query_address);
    interface.on_select_address(select_address);
    interface.on_ping(ping);
    interface.on_read_begin(read_begin);
    interface.on_read_end(read_end);
    interface.on_read_byte(read_byte);
    interface.on_read_page(read_page);
    interface.on_write_begin(write_begin);
    interface.on_write_end(write_end);
    interface.on_write_byte(write_byte);
    interface.on_protected_write_byte(protected_write_byte);
    interface.on_unprotect_and_write_byte(unprotect_and_write_byte);
    interface.on_write_page(write_page);
    interface.on_protected_write_page(protected_write_page);
    interface.on_unprotect_and_write_page(unprotect_and_write_page);

    eeprom.seek(0);
}

void loop() {
    interface.await();
}