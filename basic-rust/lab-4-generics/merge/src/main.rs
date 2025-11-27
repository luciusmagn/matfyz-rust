pub struct Merge<I1, I2> {
    left: I1,
    right: I2,
    // TODO: add more elements for logic
    switch: bool,
}

impl<T, I1, I2> From<(I1, I2)> for Merge<I1::IntoIter, I2::IntoIter>
where
    I1: IntoIterator<Item = T>,
    I2: IntoIterator<Item = T>,
{
    fn from((t1, t2): (I1, I2)) -> Self {
        Self {
            left: t1.into_iter(),
            right: t2.into_iter(),
            switch: false,
        }
    }
}

impl<T, I1, I2> Iterator for Merge<I1, I2>
where
    I1: Iterator<Item = T>,
    I2: Iterator<Item = T>,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        let ret = match self.switch {
            false => self.left.next().or_else(|| self.right.next()),
            true => self.right.next().or_else(|| self.left.next()),
        };
        self.switch = !self.switch;
        ret
    }
}

#[test]
fn left_longer() {
    let merged: Vec<_> = Merge::from((vec![1, 3, 5, 7], vec![2, 4])).collect();
    assert_eq!(merged, vec![1, 2, 3, 4, 5, 7]);
}

#[test]
fn right_longer() {
    let merged: Vec<_> = Merge::from((vec![1, 3], vec![2, 4, 6, 8])).collect();
    assert_eq!(merged, vec![1, 2, 3, 4, 6, 8]);
}

#[test]
fn left_empty() {
    let merged: Vec<_> = Merge::from((vec![], vec![1, 2, 3])).collect();
    assert_eq!(merged, vec![1, 2, 3]);
}

fn main() {}
