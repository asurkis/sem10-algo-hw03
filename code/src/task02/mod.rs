#[cfg(test)]
#[path = "./tests.rs"]
mod tests;

const MOD: u64 = 100; // 1_000_000_007;

#[derive(Debug)]
struct Node {
    product: u64,
    pending_x: u64,
    pending_t: usize,
    len: usize,
    left: Option<Box<Node>>,
    right: Option<Box<Node>>,
}

fn calc_pow(x: u64, n: u64) -> u64 {
    // O(log n)
    if n == 0 {
        1
    } else {
        let xx = calc_pow(x, n / 2);
        let xx = xx * xx % MOD;
        if n % 2 == 0 {
            xx
        } else {
            xx * x % MOD
        }
    }
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
                left.product * right.product % MOD,
                Some(Box::new(left)),
                Some(Box::new(right)),
            )
        };
        Self {
            product,
            pending_x: 0,
            pending_t: 0,
            len: slice.len(),
            left,
            right,
        }
    }

    fn update_full(&mut self, x: u64, t: usize) {
        // O(log n)
        if t > self.pending_t {
            self.product = calc_pow(x, self.len as u64);
            self.pending_x = x;
            self.pending_t = t;
        }
    }

    fn push(&mut self) {
        // O(1)
        for node in &mut self.left {
            node.update_full(self.pending_x, self.pending_t);
        }
        for node in &mut self.right {
            node.update_full(self.pending_x, self.pending_t);
        }
        self.pending_x = 0;
        self.pending_t = 0;
    }

    fn product(&mut self, mut l: usize, mut r: usize) -> u64 {
        self.push(); // O(1)
        if l >= self.len || r == 0 {
            return 1; // O(1)
        }
        if l == 0 && r >= self.len {
            return self.product; // O(1)
        }
        // O(log n), по свойству дерева отрезков
        let mut result = 1;
        for node in &mut self.left {
            result = node.product(l, r);
            l -= node.len.min(l);
            r -= node.len.min(r);
        }
        for node in &mut self.right {
            result *= node.product(l, r);
        }
        result % MOD
    }

    fn update(&mut self, mut l: usize, mut r: usize, mut x: u64, mut t: usize) {
        if t < self.pending_t {
            // O(1)
            x = self.pending_x;
            t = self.pending_t;
        } else {
            self.push(); // O(1)
        }
        if l >= self.len || r == 0 {
            return; // O(1)
        }
        if l == 0 && r >= self.len {
            // O(log n), не чаще 1 раза на запрос
            self.update_full(x, t);
            return;
        }
        // O(log n) по свойству дерева отрезков
        self.product = 1;
        for node in &mut self.left {
            node.update(l, r, x, t);
            l -= node.len.min(l);
            r -= node.len.min(r);
            self.product = node.product;
        }
        for node in &mut self.right {
            node.update(l, r, x, t);
            self.product *= node.product;
        }
        self.product %= MOD;
    }
}

#[derive(Debug)]
pub struct Tree {
    root: Node,
    time: usize,
}

impl Tree {
    pub fn new(slice: &[u64]) -> Self {
        Self {
            root: Node::new(slice),
            time: 1,
        }
    }

    pub fn product(&mut self, l: usize, r: usize) -> u64 {
        self.root.product(l, r + 1)
    }

    pub fn update(&mut self, l: usize, r: usize, x: u64) {
        self.root.update(l, r + 1, x, self.time);
        self.time += 1;
    }
}
