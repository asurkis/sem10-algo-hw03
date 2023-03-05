#[cfg(test)]
#[path = "./tests.rs"]
mod tests;

use std::collections::VecDeque;

pub struct Solution {
    k: usize,
    fixed: Vec<i64>,
    last: VecDeque<i64>,
    rev: bool,
}

impl Solution {
    pub fn new(k: usize) -> Self {
        Self {
            k,
            fixed: Vec::new(),
            last: VecDeque::new(),
            rev: false,
        }
    }

    pub fn append(&mut self, x: i64) {
        if self.last.len() >= self.k {
            let y = if self.rev {
                self.last.pop_back() // O(1)
            } else {
                self.last.pop_front() // O(1)
            };
            self.fixed.push(y.unwrap()); // O(1)
        }
        if self.rev {
            self.last.push_front(x); // O(1)
        } else {
            self.last.push_back(x); // O(1)
        }
    }

    pub fn rev(&mut self) {
        self.rev ^= true;
    }

    pub fn result(mut self) -> Vec<i64> {
        // Добиваем оставшиеся, O(k)
        if self.rev {
            for &x in self.last.iter().rev() {
                self.fixed.push(x);
            }
        } else {
            for &x in self.last.iter() {
                self.fixed.push(x);
            }
        }
        self.fixed
    }
}
