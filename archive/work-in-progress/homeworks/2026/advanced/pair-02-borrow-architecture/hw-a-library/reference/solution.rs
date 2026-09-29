pub struct RowView<'a> {
    fields: Vec<&'a str>,
}

impl<'a> RowView<'a> {
    pub fn new(line: &'a str, sep: char) -> Self {
        let fields = if line.is_empty() {
            Vec::new()
        } else {
            line.split(sep).collect()
        };

        Self { fields }
    }

    pub fn field_count(&self) -> usize {
        self.fields.len()
    }

    pub fn get(&self, idx: usize) -> Option<&'a str> {
        self.fields.get(idx).copied()
    }

    pub fn longest_field(&self) -> Option<&'a str> {
        let mut best = None;
        let mut best_len = 0;

        for field in self.fields.iter().copied() {
            if best.is_none() || field.len() > best_len {
                best = Some(field);
                best_len = field.len();
            }
        }

        best
    }

    pub fn iter(&self) -> impl Iterator<Item = &'a str> + '_ {
        self.fields.iter().copied()
    }
}

pub fn zip_fields<'a, 'b>(left: &'a str, right: &'b str, sep: char) -> Vec<(&'a str, &'b str)> {
    let left = RowView::new(left, sep);
    let right = RowView::new(right, sep);

    left.iter()
        .zip(right.iter())
        .collect()
}
