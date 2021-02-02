use std::ops::{Deref, DerefMut};

use crate::error::{SimulationResult, SimulationErrorKind, SimulationError};

pub struct InstructionQueue {
    queue: [u8; 6],
    base_pointer: usize
}
impl InstructionQueue {
    pub fn new() -> Self {
        InstructionQueue {
            queue: [0; 6],
            base_pointer: 0
        }
    }

    pub fn push(&mut self, word: (u8, u8)) -> SimulationResult<()> {
        if self.base_pointer > 4 {
            Err(SimulationError::from(SimulationErrorKind::InstructionQueueFull))
        } else {
            self.queue[self.base_pointer] = word.0;
            self.queue[self.base_pointer + 1] = word.1;
            self.base_pointer += 2;
            Ok(())
        }
    }

    pub fn push_byte(&mut self, byte: u8) -> SimulationResult<()> {
        if self.base_pointer > 4 {
            Err(SimulationError::from(SimulationErrorKind::InstructionQueueFull))
        } else {
            self.queue[self.base_pointer] = byte;
            self.base_pointer += 1;
            Ok(())
        }
    }

    pub fn pop(&mut self) -> Option<u8> {
        let result = self.queue[0];
        for i in 0..5 {
            self.queue[i] = self.queue[i + 1];
        }
        if self.base_pointer > 0 {
            self.base_pointer -= 1;
            Some(result)
        } else {
            None
        }
    }

    pub fn clear(&mut self) {
        self.base_pointer = 0;
    }

    pub fn is_full(&self) -> bool {
        self.base_pointer > 4
    }

    pub fn is_empty(&self) -> bool {
        self.base_pointer == 0
    }

    pub fn len(&self) -> usize {
        self.base_pointer
    }
}
impl Deref for InstructionQueue {
    type Target = u8;

    fn deref(&self) -> &Self::Target {
        &self.queue[0]
    }
}
impl DerefMut for InstructionQueue {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.queue[0]
    }
}

#[cfg(test)]
mod tests {
    use crate::biu::queue::InstructionQueue;

    #[test]
    fn test_queue() {
        let mut queue = InstructionQueue::new();

        assert_eq!(queue.len(), 0);
        assert!(queue.is_empty());
        assert!(!queue.is_full());
        assert!(queue.pop().is_none());

        queue.push((0x01, 0x02)).unwrap();

        assert_eq!(queue.len(), 2);
        assert!(!queue.is_empty());
        assert!(!queue.is_full());
        assert_eq!(queue.pop(), Some(0x01));
        assert_eq!(queue.len(), 1);

        queue.push((0x03, 0x04)).unwrap();
        queue.push((0x05, 0x06)).unwrap();

        assert_eq!(queue.len(), 5);
        assert!(!queue.is_empty());
        assert!(queue.is_full());
        assert!(queue.push((0x07, 0x08)).is_err());
        assert!(queue.push_byte(0x07).is_err());

        assert_eq!(queue.len(), 5);
        assert_eq!(queue.pop(), Some(0x02));
        assert!(!queue.is_full());
        assert!(queue.push_byte(0x07).is_ok());
        assert_eq!(queue.len(), 5);

        queue.clear();
        assert_eq!(queue.len(), 0);
        assert!(queue.is_empty());
        assert!(!queue.is_full());
        assert!(queue.pop().is_none());
    }
}