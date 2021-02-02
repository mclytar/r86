#ifndef __BUTTON_H
#define __BUTTON_H

#include <stdint.h>

class Button {
    uint8_t _pin;
    uint8_t _status;
    uint32_t _lastChangeTime;
public:
    Button(uint8_t pin);

    bool pressed();
    bool long_pressed();
    bool changed_status();
    void update();
};

#endif