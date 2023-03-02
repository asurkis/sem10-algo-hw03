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

struct Leaf {
    x: u64,
    t: usize,
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
struct Tree {
    root: Node,
    time: usize,
}

impl Tree {
    fn new(slice: &[u64]) -> Self {
        Self {
            root: Node::new(slice),
            time: 1,
        }
    }

    fn product(&mut self, l: usize, r: usize) -> u64 {
        self.root.product(l, r + 1)
    }

    fn update(&mut self, l: usize, r: usize, x: u64) {
        self.root.update(l, r + 1, x, self.time);
        self.time += 1;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    #[test]
    fn test1() {
        let mut arr = [1, 2, 3, 4];
        let mut tree = Tree::new(&arr);
        for i in 0..4 {
            let mut expected = 1;
            for j in i..4 {
                expected *= arr[j];
                assert_eq!(expected, tree.product(i, j));
            }
        }
        for i in 0..4 {
            arr[i] = 4 - i as u64;
            tree.update(i, 3, arr[i]);
        }
        for i in 0..4 {
            let mut expected = 1;
            for j in i..4 {
                expected *= arr[j];
                assert_eq!(expected, tree.product(i, j));
            }
        }
    }

    fn gen_case(
        max_n: usize,
        max_m: usize,
    ) -> impl Strategy<Value = (Vec<u64>, Vec<(bool, u64, usize, usize)>)> {
        (1..max_n).prop_flat_map(move |n| {
            (1..max_m).prop_flat_map(move |m| {
                (
                    vec![0..MOD; n].prop_map(move |x| x),
                    vec![(any::<bool>(), 0..MOD, 0..n, 0..n); m].prop_map(move |v| {
                        v.iter()
                            .map(move |&(q, x, l, r)| (q, x, l.min(r), l.max(r)))
                            .collect::<Vec<_>>()
                    }),
                )
            })
        })
    }

    proptest! {
        #[test]
        fn test_equiv((array, queries) in gen_case(100, 100)) {
            let mut tree = Tree::new(&array);
            let mut naive = array;
            for (q, x, l, r) in queries {
                if q {
                    tree.update(l, r, x);
                    for i in l..=r {
                        naive[i] = x;
                    }
                } else {
                    let mut expected = 1;
                    for i in l..=r {
                        expected = expected * naive[i] % MOD;
                    }
                    let got = tree.product(l, r);
                    assert_eq!(expected, got);
                }
            }
        }
    }
}
