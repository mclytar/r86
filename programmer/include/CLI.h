#ifndef __CLI_H
#define __CLI_H

#include "AT28C256.h"

#define ERROR_OK                    0x00
#define ERROR_MULTIPART             0x01
#define ERROR_READY                 0x02
#define ERROR_END_OF_FILE           0xFB
#define ERROR_OUT_OF_RANGE          0xFC
#define ERROR_TIMEOUT               0xFD
#define ERROR_UNDEFINED             0xFE
#define ERROR_UNIMPLEMENTED         0xFF

typedef u8 (* EventHandler)();
typedef u8 (* ByteEventHandler)(u8 * data);
typedef u8 (* WordEventHandler)(u16 * data);
typedef u8 (* ReadByteEventHandler)(u16 * addr, u8 * data);
typedef u8 (* ReadPageEventHandler)(u16 page, u8 * buffer);
typedef u8 (* WriteByteEventHandler)(u16 * addr, u8 * data);
typedef u8 (* WritePageEventHandler)(u16 page, u8 * buffer);


#define DECLARE_EVENT(NAME, HANDLER, ARGS) class NAME { \
        HANDLER _handler; \
    public: \
        NAME(); \
        NAME(HANDLER); \
        NAME & operator = (HANDLER); \
        u8 operator () ARGS; \
    }

DECLARE_EVENT(Event, EventHandler, ());
DECLARE_EVENT(ByteEvent, ByteEventHandler, (u8 * data));
DECLARE_EVENT(WordEvent, WordEventHandler, (u16 * data));
DECLARE_EVENT(ReadByteEvent, ReadByteEventHandler, (u16 * addr, u8 * data));
DECLARE_EVENT(ReadPageEvent, ReadPageEventHandler, (u16 page, u8 * buffer));
DECLARE_EVENT(WriteByteEvent, WriteByteEventHandler, (u16 * addr, u8 * data));
DECLARE_EVENT(WritePageEvent, WritePageEventHandler, (u16 page, u8 * buffer));


class Interface {
    Event _chip_enable;
    Event _chip_disable;
    Event _read_begin;
    Event _read_end;
    Event _write_begin;
    Event _write_end;
    WordEvent _query_address;
    WordEvent _select_address;
    WordEvent _ping;
    ReadByteEvent _read_byte;
    ReadPageEvent _read_page;
    WriteByteEvent _write_byte;
    WriteByteEvent _protected_write_byte;
    WriteByteEvent _unprotect_and_write_byte;
    WritePageEvent _write_page;
    WritePageEvent _protected_write_page;
    WritePageEvent _unprotect_and_write_page;

    u8 _execute(u8 command, u8 * buffer, u8 * len);

public:
    Interface();

    void await();

    void on_chip_enable(EventHandler);
    void on_chip_disable(EventHandler);
    void on_query_address(WordEventHandler);
    void on_select_address(WordEventHandler);
    void on_ping(WordEventHandler);
    void on_read_begin(EventHandler);
    void on_read_end(EventHandler);
    void on_write_begin(EventHandler);
    void on_write_end(EventHandler);
    void on_read_byte(ReadByteEventHandler);
    void on_read_page(ReadPageEventHandler);
    void on_write_byte(WriteByteEventHandler);
    void on_protected_write_byte(WriteByteEventHandler);
    void on_unprotect_and_write_byte(WriteByteEventHandler);
    void on_write_page(WritePageEventHandler);
    void on_protected_write_page(WritePageEventHandler);
    void on_unprotect_and_write_page(WritePageEventHandler);
};

#endif