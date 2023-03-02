#[cfg(test)]
#[path = "./tests.rs"]
mod tests;

use std::mem::swap;

type BNode = Option<Box<Node>>;

pub struct Solution {
    root: BNode,
}

struct Node {
    x: i64,
    len: usize,
    pending_rev: bool,

    // Для поддержания АВЛ
    height: usize,
    left: BNode,
    right: BNode,
}

impl Solution {
    pub fn new() -> Self {
        Self { root: None }
    }

    pub fn append(&mut self, x: i64) {
        let mut root_opt = None;
        swap(&mut root_opt, &mut self.root);
        self.root = Some(Box::new(if let Some(root) = root_opt {
            root.append(x)
        } else {
            Node::new(x)
        }));
    }

    pub fn rev(&mut self, k: usize) {
        for node in &mut self.root {
            node.rev(k);
        }
    }

    pub fn result(self) -> Vec<i64> {
        let mut result = vec![];
        if let Some(node) = self.root {
            node.to_vec(&mut result);
        }
        result
    }
}

impl Node {
    fn new(x: i64) -> Self {
        Self {
            x,
            len: 1,
            pending_rev: false,
            height: 1,
            left: None,
            right: None,
        }
    }

    fn push_rev(&mut self) {
        if self.pending_rev {
            swap(&mut self.left, &mut self.right);
            for node in &mut self.left {
                node.pending_rev = !node.pending_rev;
            }
            for node in &mut self.right {
                node.pending_rev = !node.pending_rev;
            }
            self.pending_rev = false;
        }
    }

    fn get_height(of: &Option<Box<Node>>) -> usize {
        if let Some(x) = of {
            x.height
        } else {
            0
        }
    }

    fn rebalance_right(mut self) -> Self {
        /* // Ребалансировка АВЛ
        let hl = Self::get_height(&self.left);
        if let Some(right) = self.right {
            let hr = right.height;
            if hl + 1 >= hr {
                return self;
            }
            let hrl = Self::get_height(&right.left);
            let hrr = Self::get_height(&right.right);
            if hrl < hrr {
                Self {
                    x: right.x,
                    pending_rev: false, // До этого произошёл push_rev
                    height: hrr + 1,
                    left: Some(Box::new(Self {
                        x: self.x,
                        pending_rev: false, // До этого произошёл push_rev
                        height: hl /* == hrl */ + 1,
                        left: self.left,
                        right: right.left,
                    })),
                    right: right.right,
                }
            } else {
                self
            }
        } else {
            self
        } */
        self
    }

    fn append(mut self, x: i64) -> Self {
        self.push_rev();
        self.right = Some(Box::new(if let Some(right) = self.right {
            right.append(x)
        } else {
            Node::new(x)
        }));
        self.rebalance_right()
    }

    fn split(self, k: usize) -> (BNode, BNode) {
        unimplemented!()
    }

    fn merge(self, right: Self, k: usize) -> Self {
        unimplemented!()
    }

    fn rev(&mut self, mut k: usize) {
        if k >= self.len {
            self.pending_rev = !self.pending_rev;
        } else if k > 0 {
            self.push_rev();
            for node in &mut self.right {
                node.rev(k);
                k -= node.len;
            }
            for node in &mut self.right {
                node.rev(k);
            }
        }
    }

    fn to_vec(mut self, out: &mut Vec<i64>) {
        self.push_rev();
        if let Some(node) = self.left {
            node.to_vec(out);
        }
        out.push(self.x);
        if let Some(node) = self.right {
            node.to_vec(out);
        }
    }
}
