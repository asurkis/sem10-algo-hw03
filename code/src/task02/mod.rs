#[cfg(test)]
#[path = "./tests.rs"]
mod tests;

pub const MOD: u64 = 1_000_000_007;

#[derive(Debug)]
pub struct Solution {
    len: usize,
    root: Node,
}

impl Solution {
    pub fn new(mut vec: Vec<u64>) -> Self {
        let len = vec.len();
        // Добиваем до 2^k, очевидно,
        // n = Theta(2^ceil(log2(n)))
        while vec.len() & (vec.len() - 1) != 0 {
            vec.push(1);
        }
        Self {
            len,
            root: Node::new(&vec),
        }
    }

    pub fn product(&mut self, l: usize, r: usize) -> u64 {
        self.root.product(l, r + 1)
    }

    pub fn update(&mut self, l: usize, r: usize, x: u64) {
        // Записываем x * 2^k в список, пока 2^k не превысит длину
        // Очевидно, O(log n)
        let mut list = List {
            head: x,
            tail: None,
        };
        let mut k = 0;
        while 1 << k < self.len {
            let head = list.head * list.head % MOD;
            list = List {
                head,
                tail: Some(Rc::new(list)),
            };
            k += 1;
        }
        dbg!(&list);
        self.root.update(l, r + 1, Rc::new(list));
    }
}

use std::rc::Rc;

// Одновсязный список
#[derive(Debug, Clone)]
struct List {
    head: u64,
    tail: Option<Rc<List>>,
}

#[derive(Debug)]
struct Node {
    // Произведение на всём поддереве, поддерживаем инвариант:
    // при выходе из поддерева произведение на нём актуально.
    // Последний не отправленный ниже по дереву запрос, покрывающий всё дерево.
    // Голова списка --- произведение на поддереве.
    pending: Rc<List>,
    len: usize,
    left: Option<Box<Node>>,
    right: Option<Box<Node>>,
}

impl Node {
    fn new(slice: &[u64]) -> Self {
        let mid = slice.len() / 2;
        let (product, left, right) = if slice.len() == 1 {
            (slice[0], None, None)
        } else {
            let left = Self::new(&slice[..mid]);
            let right = Self::new(&slice[mid..]);
            (
                left.pending.head * right.pending.head,
                Some(Box::new(left)),
                Some(Box::new(right)),
            )
        };
        Self {
            pending: Rc::new(List {
                head: product % MOD,
                tail: None,
            }),
            len: slice.len(),
            left,
            right,
        }
    }

    fn push(&mut self) {
        // O(1)
        if let Some(tail) = &self.pending.tail {
            if let Some(n) = &mut self.left {
                n.pending = tail.clone();
            }
            if let Some(n) = &mut self.right {
                n.pending = tail.clone();
            }
        }
    }

    fn product(&mut self, l: usize, r: usize) -> u64 {
        // Спуск по дереву отрезков --- O(log n) на запрос
        self.push();
        if r == 0 || l >= self.len {
            return 1;
        }
        if l == 0 && r >= self.len {
            return self.pending.head;
        }
        let left = if let Some(n) = &mut self.left {
            n.product(l, r)
        } else {
            1
        };
        let right = if let Some(n) = &mut self.right {
            let llen = self.len / 2;
            n.product(l.max(llen) - llen, r.max(llen) - llen)
        } else {
            1
        };
        left * right % MOD
    }

    fn update(&mut self, l: usize, r: usize, x: Rc<List>) {
        // Спуск по дереву отрезков, в каждой вершине действия за O(1),
        // всего --- O(log n) на запрос
        self.push();
        if r == 0 || l >= self.len {
            return;
        }
        if l == 0 && r >= self.len {
            self.pending = x;
            return;
        }

        let left = if let Some(n) = &mut self.left {
            n.update(l, r, x.tail.clone().unwrap());
            n.pending.head
        } else {
            1
        };
        let right = if let Some(n) = &mut self.right {
            let llen = self.len / 2;
            n.update(
                l.max(llen) - llen,
                r.max(llen) - llen,
                x.tail.clone().unwrap(),
            );
            n.pending.head
        } else {
            1
        };

        self.pending = Rc::new(List {
            head: left * right % MOD,
            tail: None,
        });
    }
}
