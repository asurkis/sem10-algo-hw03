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
