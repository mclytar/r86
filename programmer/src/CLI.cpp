#include <Arduino.h>

#include "CLI.h"

#define COMMAND_NONE                0x00
#define COMMAND_CHIP_ENABLE         0x01
#define COMMAND_CHIP_DISABLE        0x02
#define COMMAND_QUERY_ADDRESS       0x03
#define COMMAND_SELECT_ADDRESS      0x04
#define COMMAND_PING                0x05

#define COMMAND_READ_BYTE           0x10
#define COMMAND_READ_BYTE_AT        0x11
#define COMMAND_READ_PAGE           0x12
#define COMMAND_READ_ALL            0x13

#define COMMAND_WRITE_BYTE          0x20
#define COMMAND_WRITE_BYTE_AT       0x21
#define COMMAND_WRITE_PAGE          0x22
#define COMMAND_WRITE_ALL           0x23
#define COMMAND_P_WRITE_BYTE        0x24
#define COMMAND_P_WRITE_BYTE_AT     0x25
#define COMMAND_P_WRITE_PAGE        0x26
#define COMMAND_P_WRITE_ALL         0x27
#define COMMAND_UP_WRITE_BYTE       0x28
#define COMMAND_UP_WRITE_BYTE_AT    0x29
#define COMMAND_UP_WRITE_PAGE       0x2A
#define COMMAND_UP_WRITE_ALL        0x2B

#define SIGNAL_CONTENT              0x80
#define SIGNAL_EOF                  0x81
#define SIGNAL_CONTINUE             0x82
#define SIGNAL_ABORT                0x83

#define IMPLEMENT_EVENT(NAME, HANDLER, ARGS, ARGSVAL) \
    NAME::NAME() { this->_handler = nullptr; } \
    NAME::NAME(HANDLER h) { this->_handler = h; } \
    NAME & NAME::operator = (HANDLER h) { this->_handler = h; return *this; } \
    u8 NAME::operator () ARGS { if (this->_handler) return this->_handler ARGSVAL; else return ERROR_UNIMPLEMENTED; }

#define TRY(INSTR) { u8 __result = (INSTR); if (__result != ERROR_OK) return __result; }
#define TRY_OPTIONAL(INSTR) { u8 __result = (INSTR); if (__result != ERROR_OK && __result != ERROR_UNIMPLEMENTED) return __result; }
#define TRY_CATCH(INSTR, FALLBACK) { u8 __result = (INSTR); if (__result != ERROR_OK) { (FALLBACK); return __result; }}

IMPLEMENT_EVENT(Event, EventHandler, (), ());
IMPLEMENT_EVENT(ByteEvent, ByteEventHandler, (u8 * data), (data));
IMPLEMENT_EVENT(WordEvent, WordEventHandler, (u16 * data), (data));
IMPLEMENT_EVENT(ReadByteEvent, ReadByteEventHandler, (u16 * addr, u8 * data), (addr, data));
IMPLEMENT_EVENT(ReadPageEvent, ReadPageEventHandler, (u16 addr, u8 * buffer), (addr, buffer));
IMPLEMENT_EVENT(WriteByteEvent, WriteByteEventHandler, (u16 * addr, u8 * data), (addr, data));
IMPLEMENT_EVENT(WritePageEvent, WritePageEventHandler, (u16 addr, u8 * buffer), (addr, buffer));


Interface::Interface() {}

u8 Interface::_execute(u8 command, u8 * buffer, u8 * size) {
    switch (command) {
        case COMMAND_NONE:
            return ERROR_OK;
        case COMMAND_CHIP_ENABLE:
            return this->_chip_enable();
        case COMMAND_CHIP_DISABLE:
            return this->_chip_disable();
        case COMMAND_QUERY_ADDRESS:
            {
                u16 address = 0;
                TRY(this->_query_address(&address));
                *((u16 *)buffer) = address;
                *size = 2;
            }
            return ERROR_OK;
        case COMMAND_SELECT_ADDRESS:
            {
                u16 address = 0;
                u16 len = Serial.readBytes((u8 *)&address, 2);
                if (len != 2) return ERROR_TIMEOUT;
                TRY(this->_select_address(&address));
                *((u16 *)buffer) = address;
                *size = 2;
            }
            return ERROR_OK;
        case COMMAND_PING:
            {
                u16 data = 0;
                TRY(this->_ping(&data));
                *((u16 *)buffer) = data;
                *size = 2;
            }
            return ERROR_OK;
        case COMMAND_READ_BYTE:
            {
                u8 data = 0;
                TRY_OPTIONAL(this->_read_begin());
                TRY_CATCH(this->_read_byte(nullptr, &data), this->_read_end());
                TRY_OPTIONAL(this->_read_end());
                *buffer = data;
                *size = 1;
            }
            return ERROR_OK;
        case COMMAND_READ_BYTE_AT:
            {
                u8 data = 0;
                u16 address = 0;
                u16 len = Serial.readBytes((u8 *)&address, 2);
                if (len != 2) return ERROR_TIMEOUT;
                TRY_OPTIONAL(this->_read_begin());
                TRY_CATCH(this->_read_byte(&address, &data), this->_read_end());
                TRY_OPTIONAL(this->_read_end());
                *buffer = data;
                *size = 1;
            }
            return ERROR_OK;
        case COMMAND_READ_PAGE:
            {
                u8 response;
                u16 page = 0;
                u16 len = Serial.readBytes((u8 *)&page, 2);
                if (len != 2) return ERROR_TIMEOUT;
                TRY_OPTIONAL(this->_read_begin());
                TRY_CATCH(this->_read_page(page, buffer), this->_read_end());

                response = ERROR_MULTIPART;
                Serial.write(&response, 1);
                Serial.write(buffer, 64);
                Serial.flush();

                Serial.readBytes(&response, 1);
                if ((response != SIGNAL_CONTINUE) && (response != SIGNAL_ABORT)) {
                    this->_read_end();
                    return ERROR_UNDEFINED;
                }

                TRY_OPTIONAL(this->_read_end());
            }
            return ERROR_OK;
        case COMMAND_READ_ALL:
            {
                u8 response;
                TRY_OPTIONAL(this->_read_begin());
                for (u16 page = 0; page < 512; page++) {
                    TRY_CATCH(this->_read_page(page, buffer), this->_read_end());

                    response = ERROR_MULTIPART;
                    Serial.write(&response, 1);
                    Serial.write(buffer, 64);
                    Serial.flush();

                    Serial.readBytes(&response, 1);
                    if (response == SIGNAL_CONTINUE) continue;
                    if (response == SIGNAL_ABORT) break;

                    this->_read_end();
                    return ERROR_UNDEFINED;
                }
                TRY_OPTIONAL(this->_read_end());
            }
            return ERROR_OK;
        case COMMAND_WRITE_BYTE:
            {
                u8 data = 0;
                u16 len = Serial.readBytes(&data, 1);
                if (len != 1) return ERROR_TIMEOUT;
                TRY_OPTIONAL(this->_write_begin());
                TRY_CATCH(this->_write_byte(nullptr, &data), this->_write_end());
                TRY_OPTIONAL(this->_write_end());
            }
            return ERROR_OK;
        case COMMAND_P_WRITE_BYTE:
            {
                u8 data = 0;
                u16 len = Serial.readBytes(&data, 1);
                if (len != 1) return ERROR_TIMEOUT;
                TRY_OPTIONAL(this->_write_begin());
                TRY_CATCH(this->_protected_write_byte(nullptr, &data), this->_write_end());
                TRY_OPTIONAL(this->_write_end());
            }
            return ERROR_OK;
        case COMMAND_UP_WRITE_BYTE:
            {
                u8 data = 0;
                u16 len = Serial.readBytes(&data, 1);
                if (len != 1) return ERROR_TIMEOUT;
                TRY_OPTIONAL(this->_write_begin());
                TRY_CATCH(this->_unprotect_and_write_byte(nullptr, &data), this->_write_end());
                TRY_OPTIONAL(this->_write_end());
            }
            return ERROR_OK;
        case COMMAND_WRITE_BYTE_AT:
            {
                u8 data = 0;
                u16 address = 0;
                u16 len = Serial.readBytes((u8 *)&address, 2);
                if (len != 2) return ERROR_TIMEOUT;
                len = Serial.readBytes(&data, 1);
                if (len != 1) return ERROR_TIMEOUT;
                TRY_OPTIONAL(this->_write_begin());
                TRY_CATCH(this->_write_byte(&address, &data), this->_write_end());
                TRY_OPTIONAL(this->_write_end());
            }
            return ERROR_OK;
        case COMMAND_P_WRITE_BYTE_AT:
            {
                u8 data = 0;
                u16 address = 0;
                u16 len = Serial.readBytes((u8 *)&address, 2);
                if (len != 2) return ERROR_TIMEOUT;
                len = Serial.readBytes(&data, 1);
                if (len != 1) return ERROR_TIMEOUT;
                TRY_OPTIONAL(this->_write_begin());
                TRY_CATCH(this->_protected_write_byte(&address, &data), this->_write_end());
                TRY_OPTIONAL(this->_write_end());
            }
            return ERROR_OK;
        case COMMAND_UP_WRITE_BYTE_AT:
            {
                u8 data = 0;
                u16 address = 0;
                u16 len = Serial.readBytes((u8 *)&address, 2);
                if (len != 2) return ERROR_TIMEOUT;
                len = Serial.readBytes(&data, 1);
                if (len != 1) return ERROR_TIMEOUT;
                TRY_OPTIONAL(this->_write_begin());
                TRY_CATCH(this->_unprotect_and_write_byte(&address, &data), this->_write_end());
                TRY_OPTIONAL(this->_write_end());
            }
            return ERROR_OK;
        case COMMAND_WRITE_PAGE:
            {
                u8 response;
                u16 page = 0;
                u16 len = Serial.readBytes((u8 *)&page, 2);
                if (len != 2) return ERROR_TIMEOUT;
                
                response = ERROR_READY;
                Serial.write(&response, 1);
                Serial.flush();
                len = Serial.readBytes(buffer, 64);
                if (len != 64) return ERROR_TIMEOUT;

                TRY_OPTIONAL(this->_write_begin());
                TRY_CATCH(this->_write_page(page, buffer), this->_write_end());
                TRY_OPTIONAL(this->_write_end());
            }
            return ERROR_OK;
        case COMMAND_P_WRITE_PAGE:
            {
                u8 response;
                u16 page = 0;
                u16 len = Serial.readBytes((u8 *)&page, 2);
                if (len != 2) return ERROR_TIMEOUT;
                
                response = ERROR_READY;
                Serial.write(&response, 1);
                Serial.flush();
                len = Serial.readBytes(buffer, 64);
                if (len != 64) return ERROR_TIMEOUT;

                TRY_OPTIONAL(this->_write_begin());
                TRY_CATCH(this->_protected_write_page(page, buffer), this->_write_end());
                TRY_OPTIONAL(this->_write_end());
            }
            return ERROR_OK;
        case COMMAND_UP_WRITE_PAGE:
            {
                u8 response;
                u16 page = 0;
                u16 len = Serial.readBytes((u8 *)&page, 2);
                if (len != 2) return ERROR_TIMEOUT;
                
                response = ERROR_READY;
                Serial.write(&response, 1);
                Serial.flush();
                len = Serial.readBytes(buffer, 64);
                if (len != 64) return ERROR_TIMEOUT;

                TRY_OPTIONAL(this->_write_begin());
                TRY_CATCH(this->_unprotect_and_write_page(page, buffer), this->_write_end());
                TRY_OPTIONAL(this->_write_end());
            }
            return ERROR_OK;
        case COMMAND_WRITE_ALL:
            {
                u8 response;
                TRY_OPTIONAL(this->_write_begin());
                for (u16 page = 0; page < 512; page++) {
                    response = ERROR_READY;
                    Serial.write(&response, 1);
                    Serial.flush();
                    u16 len = Serial.readBytes(buffer, 64);
                    if (len != 64) return ERROR_TIMEOUT;

                    TRY_CATCH(this->_write_page(page, buffer), this->_read_end());
                }
                TRY_OPTIONAL(this->_write_end());
            }
            return ERROR_OK;
        case COMMAND_P_WRITE_ALL:
            {
                u8 response;
                TRY_OPTIONAL(this->_write_begin());
                for (u16 page = 0; page < 512; page++) {
                    response = ERROR_READY;
                    Serial.write(&response, 1);
                    Serial.flush();
                    u16 len = Serial.readBytes(buffer, 64);
                    if (len != 64) return ERROR_TIMEOUT;

                    TRY_CATCH(this->_protected_write_page(page, buffer), this->_read_end());
                }
                TRY_OPTIONAL(this->_write_end());
            }
            return ERROR_OK;
        case COMMAND_UP_WRITE_ALL:
            {
                u8 response;
                TRY_OPTIONAL(this->_write_begin());
                for (u16 page = 0; page < 512; page++) {
                    response = ERROR_READY;
                    Serial.write(&response, 1);
                    Serial.flush();
                    u16 len = Serial.readBytes(buffer, 64);
                    if (len != 64) return ERROR_TIMEOUT;

                    TRY_CATCH(this->_unprotect_and_write_page(page, buffer), this->_read_end());
                }
                TRY_OPTIONAL(this->_write_end());
            }
            return ERROR_OK;
        default:
            return ERROR_UNDEFINED;
    }
}

void Interface::await() {
    u8 buffer[256];
    u8 size = 0;

    while (Serial.available() == 0);
    u8 _cmd = Serial.read();
    u32 _timeout = Serial.getTimeout();
    Serial.setTimeout(1000);

    u8 _result = this->_execute(_cmd, buffer, &size);

    Serial.write(&_result, 1);
    if (_result == ERROR_OK && size > 0) Serial.write(buffer, size);

    Serial.flush();
    Serial.setTimeout(_timeout);
}

void Interface::on_chip_enable(EventHandler e) {
    this->_chip_enable = e;
}

void Interface::on_chip_disable(EventHandler e) {
    this->_chip_disable = e;
}

void Interface::on_query_address(WordEventHandler e) {
    this->_query_address = e;
}

void Interface::on_select_address(WordEventHandler e) {
    this->_select_address = e;
}

void Interface::on_ping(WordEventHandler e) {
    this->_ping = e;
}

void Interface::on_read_begin(EventHandler e) {
    this->_read_begin = e;
}

void Interface::on_read_end(EventHandler e) {
    this->_read_end = e;
}

void Interface::on_read_byte(ReadByteEventHandler e) {
    this->_read_byte = e;
}

void Interface::on_read_page(ReadPageEventHandler e) {
    this->_read_page = e;
}

void Interface::on_write_begin(EventHandler e) {
    this->_write_begin = e;
}

void Interface::on_write_end(EventHandler e) {
    this->_write_end = e;
}

void Interface::on_write_byte(WriteByteEventHandler e) {
    this->_write_byte = e;
}

void Interface::on_protected_write_byte(WriteByteEventHandler e) {
    this->_protected_write_byte = e;
}

void Interface::on_unprotect_and_write_byte(WriteByteEventHandler e) {
    this->_unprotect_and_write_byte = e;
}

void Interface::on_write_page(WritePageEventHandler e) {
    this->_write_page = e;
}

void Interface::on_protected_write_page(WritePageEventHandler e) {
    this->_protected_write_page = e;
}

void Interface::on_unprotect_and_write_page(WritePageEventHandler e) {
    this->_unprotect_and_write_page = e;
}