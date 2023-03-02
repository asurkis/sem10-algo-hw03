use super::*;

#[test]
fn test1() {
    let mut solution = Solution::new();
    let mut naive = vec![];

    let mut queries = vec![
        (false, 1),
        (false, 2),
        (false, 3),
        (true, 1),
        (true, 2),
        (true, 3),
    ];

    for (t, x) in queries {
        if t {
            solution.append(x);
            naive.push(x);
        } else {
            let k = x.abs() as usize;
            solution.rev(k);
            let k = naive.len().min(k);
            let len = naive.len();
            naive[len - k..].reverse();
        }
    }

    let got = solution.result();
    assert_eq!(naive, got);
}
