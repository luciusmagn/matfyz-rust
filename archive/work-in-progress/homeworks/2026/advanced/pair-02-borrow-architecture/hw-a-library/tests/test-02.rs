mod solution;

fn main() {
    let row = solution::RowView::new("ab|cdef|xy", '|');
    println!("{:?}", row.longest_field());

    let empty = solution::RowView::new("", '|');
    println!("{}", empty.field_count());
    println!("{:?}", empty.longest_field());
}
