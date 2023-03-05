use super::*;

#[test]
fn test1() {
    let k = 3;
    let mut solution = Solution::new(k);
    let mut naive = vec![];

    let queries = vec![
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
            solution.rev();
            let k = naive.len().min(k);
            let len = naive.len();
            naive[len - k..].reverse();
        }
    }

    let got = solution.result();
    assert_eq!(naive, got);
}
