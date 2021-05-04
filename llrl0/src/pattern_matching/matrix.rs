use super::{Pattern, Row, Tag};
use std::collections::BTreeSet;

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Hash)]
pub struct Matrix {
    rows: Vec<Row>,
}

impl Matrix {
    pub fn new(rows: Vec<Row>) -> Self {
        debug_assert!(match rows.as_slice() {
            [] => true,
            [x, xs @ ..] => xs.iter().all(|y| y.len() == x.len()),
        });
        Self { rows }
    }

    pub fn row_empty() -> Self {
        Self::new(Vec::new())
    }

    pub fn is_row_empty(&self) -> bool {
        self.rows.is_empty()
    }

    pub fn row_len(&self) -> usize {
        self.rows.len()
    }

    pub fn rows(&self) -> impl Iterator<Item = &Row> {
        self.rows.iter()
    }

    pub fn into_rows(self) -> Vec<Row> {
        self.rows
    }

    pub fn column_empty(row_len: usize) -> Self {
        Self::new(vec![Row::empty(); row_len])
    }

    pub fn is_column_empty(&self) -> Option<bool> {
        match self.rows.as_slice() {
            [] => None,
            [x, ..] => Some(x.is_empty()),
        }
    }

    pub fn column_len(&self) -> Option<usize> {
        match self.rows.as_slice() {
            [] => None,
            [x, ..] => Some(x.len()),
        }
    }

    pub fn collect_head_tags(&self) -> BTreeSet<&Tag> {
        self.rows.iter().fold(BTreeSet::new(), |mut set, row| {
            row.collect_head_tags(&mut set);
            set
        })
    }

    pub fn concat_row(mut self, other: Row) -> Self {
        debug_assert!(match self.column_len() {
            None => true,
            Some(len) => len == other.len(),
        });
        self.rows.push(other);
        self
    }

    pub fn concat_rows(mut self, mut other: Matrix) -> Self {
        debug_assert!(match (self.column_len(), other.column_len()) {
            (Some(a), Some(b)) => a == b,
            _ => true,
        });
        self.rows.append(&mut other.rows);
        self
    }

    pub fn compute_usefulness(&mut self, mut row: Row) -> bool {
        if self.is_row_empty() {
            return true;
        }

        debug_assert_eq!(self.column_len(), Some(row.len()));
        if let Some(head) = row.take_head() {
            match head {
                Pattern::Constructor(tag, ps) => {
                    self.specialize(&tag);
                    // Equivalent of (head ++ row).specialize(&tag)
                    row.append_head(ps);
                    self.compute_usefulness(row)
                }
                Pattern::Wildcard => {
                    let tags = self.collect_head_tags();
                    if Tag::is_complete(&tags) {
                        tags.into_iter().any(|tag| {
                            let mut m = self.clone();
                            let mut row = row.clone();
                            m.specialize(tag);
                            // Equivalent of (head ++ row).specialize(&tag)
                            row.append_head(vec![Pattern::Wildcard; tag.arity()]);
                            m.compute_usefulness(row)
                        })
                    } else {
                        self.defaults();
                        self.compute_usefulness(row)
                    }
                }
            }
        } else {
            false
        }
    }

    pub fn compute_missing(&mut self, column_len: usize) -> Matrix {
        if self.is_row_empty() {
            let row = Row::new(vec![Pattern::Wildcard; column_len].into());
            return Matrix::new(vec![row]);
        }

        debug_assert_eq!(self.column_len(), Some(column_len));
        if self.is_column_empty() == Some(true) {
            return Matrix::row_empty();
        }

        let tags = self.collect_head_tags();
        if Tag::is_complete(&tags) {
            return tags.into_iter().fold(Matrix::row_empty(), |result, tag| {
                let mut m = self.clone();
                m.specialize(tag);
                let mut m = m.compute_missing(column_len + tag.arity() - 1);
                m.restore_specialized(tag);
                result.concat_rows(m)
            });
        }

        let heads = match tags.iter().next() {
            None => vec![Pattern::Wildcard],
            Some(tag) => match tag.siblings() {
                None => vec![Pattern::Wildcard],
                Some(siblings) => siblings
                    .into_iter()
                    .filter(|tag| !tags.contains(tag))
                    .map(|tag| {
                        let args = vec![Pattern::Wildcard; tag.arity()];
                        Pattern::Constructor(tag, args)
                    })
                    .collect(),
            },
        };

        self.defaults();
        let tails = self.compute_missing(column_len - 1).into_rows();

        Matrix::new(
            tails
                .into_iter()
                .flat_map(|tail| {
                    heads.iter().cloned().map(move |head| {
                        let mut row = tail.clone();
                        row.insert_head(head);
                        row
                    })
                })
                .collect(),
        )
    }

    pub fn specialize(&mut self, tag: &Tag) {
        for row in std::mem::take(&mut self.rows) {
            if let Some(row) = row.specialize(tag) {
                self.rows.push(row);
            }
        }
    }

    pub fn restore_specialized(&mut self, tag: &Tag) {
        for row in self.rows.iter_mut() {
            row.restore_specialized(tag);
        }
    }

    pub fn defaults(&mut self) {
        for row in std::mem::take(&mut self.rows) {
            if let Some(row) = row.defaults() {
                self.rows.push(row);
            }
        }
    }

    pub fn clone_filter(&self, mut f: impl FnMut(&Row) -> bool) -> Self {
        Matrix::new(self.rows.iter().filter(move |x| f(x)).cloned().collect())
    }

    pub fn drain_filter(&mut self, mut f: impl FnMut(&Row) -> bool) {
        for row in std::mem::take(&mut self.rows) {
            if f(&row) {
                self.rows.push(row);
            }
        }
    }
}
