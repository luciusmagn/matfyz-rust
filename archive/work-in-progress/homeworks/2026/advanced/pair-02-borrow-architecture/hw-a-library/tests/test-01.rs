mod solution;

fn main() {
    let row = solution::RowView::new("alpha|beta|gamma", '|');
    println!("{}", row.field_count());
    println!("{:?}", row.get(1));
    println!("{:?}", row.get(9));
}
