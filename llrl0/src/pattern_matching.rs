// http://moscova.inria.fr/~maranget/papers/warn/warn.pdf

mod matrix;
mod pattern;
mod row;

pub use matrix::Matrix;
pub use pattern::{FiniteCon, Pattern, Tag};
pub use row::Row;

#[cfg(test)]
mod tests;

// TODO: Compare performance with im-rc

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Hash)]
pub enum Error {
    Useless(Row),
    NonExhaustive(Matrix),
}

pub fn check(rows: Vec<Row>) -> Result<(), Error> {
    let mut mat = Matrix::row_empty();
    for row in rows {
        mat = useless_check(mat, row).map_err(Error::Useless)?;
    }
    match exhaustiveness_check(mat) {
        rows if rows.is_row_empty() => Ok(()),
        rows => Err(Error::NonExhaustive(rows)),
    }
}

pub fn useless_check(mut mat: Matrix, row: Row) -> Result<Matrix, Row> {
    let mut related_mat = mat.clone_filter(|r| !r.is_incompatible_with(&row));
    if related_mat.compute_usefulness(row.clone()) {
        mat.drain_filter(|r| Matrix::new(vec![row.clone()]).compute_usefulness(r.clone()));
        Ok(mat.concat_row(row))
    } else {
        Err(row)
    }
}

pub fn exhaustiveness_check(mut mat: Matrix) -> Matrix {
    let l = mat.column_len().unwrap_or_default();
    mat.compute_missing(l)
}
