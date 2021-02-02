#include <Arduino.h>

#include <Button.h>

#define BTNSTATUS_NONE          0x00
#define BTNSTATUS_PRESSED       0x01
#define BTNSTATUS_CHANGED       0x02

Button::Button(uint8_t pin) {
    this->_pin = pin;
    pinMode(pin, INPUT_PULLUP);
    this->_status = BTNSTATUS_NONE;
    this->_lastChangeTime = millis();
}

void Button::update() {
    uint8_t status = digitalRead(this->_pin);

    if (status == LOW) status = BTNSTATUS_PRESSED;
    else status = BTNSTATUS_NONE;

    this->_status &= ~BTNSTATUS_CHANGED;
    this->_status |= ((this->_status ^ status) & BTNSTATUS_PRESSED) << 1;
    this->_status &= ~BTNSTATUS_PRESSED;
    this->_status |= status;

    if (this->_status & BTNSTATUS_CHANGED) {
        this->_lastChangeTime = millis();
    }
}

bool Button::pressed() {
    return this->_status & BTNSTATUS_PRESSED;
}

bool Button::long_pressed() {
    return (this->_status & BTNSTATUS_PRESSED) && (millis() > (this->_lastChangeTime + 1000));
}

bool Button::changed_status() {
    return this->_status & BTNSTATUS_CHANGED;
}