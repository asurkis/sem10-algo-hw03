use super::*;
use proptest::prelude::*;

#[test]
fn test1() {
    let mut arr = vec![1, 2, 3, 4];
    let mut solution = Solution::new(arr.clone());
    for i in 0..4 {
        let mut expected = 1;
        for j in i..4 {
            expected *= arr[j];
            assert_eq!(expected, solution.product(i, j));
        }
    }
    for i in 0..4 {
        arr[i] = 4 - i as u64;
        solution.update(i, 3, arr[i]);
    }
    for i in 0..4 {
        let mut expected = 1;
        for j in i..4 {
            expected *= arr[j];
            assert_eq!(expected, solution.product(i, j));
        }
    }
}

#[test]
fn test2() {
    // ([0, 0, 0], [(true, 121921054, 0, 2), (false, 608464107, 0, 1)])
    let mut arr = vec![0, 0, 0];
    let mut solution = Solution::new(arr.clone());
    solution.update(0, 2, 121921054);
    // [121921054, 121921054, 121921054]
    dbg!(&solution);
    assert_eq!(121921054 * 121921054 % MOD, solution.product(0, 1));
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
    fn test_equiv((array, queries) in gen_case(10, 10)) {
        let mut solution = Solution::new(array.clone());
        let mut naive = array;
        for (q, x, l, r) in queries {
            if q {
                solution.update(l, r, x);
                for i in l..=r {
                    naive[i] = x;
                }
            } else {
                let mut expected = 1;
                for i in l..=r {
                    expected = expected * naive[i] % MOD;
                }
                let got = solution.product(l, r);
                assert_eq!(expected, got);
            }
        }
    }
}
