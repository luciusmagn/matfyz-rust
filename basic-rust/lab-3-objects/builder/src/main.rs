use chrono::{NaiveDate, Utc};

#[derive(Debug)]
struct Invoice {
    title: String,
    description: String,
    account_number: String,
    seller: String,
    buyer: String,
    due_date: NaiveDate,
    // Defaults to today
    invoice_date: NaiveDate,
    rows: Vec<InvoiceRow>,
    total_price: usize,
}

#[derive(Debug)]
struct InvoiceRow {
    item: String,
    price: usize,
}

impl Invoice {
    fn builder() -> InvoiceBuilder {
        InvoiceBuilder::default()
    }
}

// TODO
#[derive(Default)]
struct InvoiceBuilder {
    title: Option<String>,
    description: Option<String>,
    account_number: Option<String>,
    seller: Option<String>,
    buyer: Option<String>,
    due_date: Option<NaiveDate>,
    // Defaults to today
    invoice_date: Option<NaiveDate>,
    rows: Vec<InvoiceRow>,
}

#[derive(Debug)]
enum BuilderError {
    InvalidOrMissingDueDate,
    MissingTitle,
    MissingDescription,
    MissingSeller,
    MissingBuyer,
    MissingAccountNumber,
}

impl InvoiceBuilder {
    fn title(mut self, title: impl ToString) -> Self {
        self.title = Some(title.to_string());
        self
    }

    fn description(mut self, description: impl ToString) -> Self {
        self.description = Some(description.to_string());
        self
    }

    fn buyer(mut self, buyer: impl ToString) -> Self {
        self.buyer = Some(buyer.to_string());
        self
    }

    fn seller(mut self, seller: impl ToString) -> Self {
        self.seller = Some(seller.to_string());
        self
    }

    fn account_number(mut self, account_number: impl ToString) -> Self {
        self.account_number = Some(account_number.to_string());
        self
    }

    fn due_date(mut self, year: i32, month: u32, day: u32) -> Self {
        self.due_date = NaiveDate::from_ymd_opt(year, month, day);
        self
    }

    fn invoice_date(mut self, year: i32, month: u32, day: u32) -> Self {
        self.due_date = NaiveDate::from_ymd_opt(year, month, day);
        self
    }

    fn add_row(mut self, item: impl ToString, price: usize) -> Self {
        self.rows.push(InvoiceRow {
            item: item.to_string(),
            price,
        });
        self
    }

    fn build(self) -> Result<Invoice, BuilderError> {
        use BuilderError::*;

        Ok(Invoice {
            title: self.title.ok_or(MissingTitle)?,
            description: self.description.ok_or(MissingDescription)?,
            account_number: self.account_number.ok_or(MissingAccountNumber)?,
            seller: self.seller.ok_or(MissingSeller)?,
            buyer: self.buyer.ok_or(MissingBuyer)?,
            due_date: self.due_date.ok_or(InvalidOrMissingDueDate)?,
            invoice_date: self.invoice_date.unwrap_or(Utc::now().date_naive()),
            total_price: self.rows.iter().map(|r| r.price).sum(),
            rows: self.rows,
        })
    }
}

// Example
// Invoice::builder()
//      .title("Food purchase")
//      .description("This is what was bought")
//      .account_number("DE03201202006676642478")
//      .seller("John")
//      .buyer("Jack")
//      .add_row("1kg Corn", 100)
//      .add_row("1l Milk", 200)
//      .due_date(2026, 1, 5)
//      .build()
//
// => Ok(Invoice {
//      title: "Food purchase",
//      description: "This is what was bought",
//      account_number: "DE03201202006676642478",
//      seller: "John",
//      buyer: "Jack",
//      invoice_date: 2025-11-13,
//      due_date: 2026-01-05,
//      rows: [
//           InvoiceRow { item: "1kg Corn", price: 100 },
//           InvoiceRow { item: "1l Milk", price: 200 },
//      ],
//      total_price: 300,
// })

fn main() {
    dbg!(
        Invoice::builder()
            .title("Food purchase")
            .description("This is what was bought")
            .account_number("DE03201202006676642478")
            .seller("John")
            .buyer("Jack")
            .add_row("1kg Corn", 100)
            .add_row("1l Milk", 200)
            .due_date(2026, 1, 5)
            .build()
    );
}
