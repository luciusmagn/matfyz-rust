mod solution;

fn main() {
    let row = solution::RowView::new("a,,b,", ',');
    println!("{}", row.iter().collect::<Vec<_>>().join("|"));

    let tie = solution::RowView::new("ab|cd|e", '|');
    println!("{:?}", tie.longest_field());
}
