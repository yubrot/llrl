use super::{Pattern, Tag};
use derive_new::new;
use std::collections::{BTreeSet, VecDeque};

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Hash, new)]
pub struct Row {
    pats: VecDeque<Pattern>,
}

impl Row {
    pub fn empty() -> Self {
        Self::new(VecDeque::new())
    }

    pub fn is_empty(&self) -> bool {
        self.pats.is_empty()
    }

    pub fn len(&self) -> usize {
        self.pats.len()
    }

    pub fn iter(&self) -> impl Iterator<Item = &Pattern> {
        self.pats.iter()
    }

    pub fn into_inner(self) -> VecDeque<Pattern> {
        self.pats
    }

    pub fn is_incompatible_with(&self, other: &Self) -> bool {
        debug_assert_eq!(self.len(), other.len());
        self.pats
            .iter()
            .zip(other.pats.iter())
            .any(|(a, b)| a.is_incompatible_with(b))
    }

    pub fn collect_head_tags<'a>(&'a self, set: &mut BTreeSet<&'a Tag>) {
        if let Some(x) = self.pats.front() {
            x.collect_head_tags(set);
        }
    }

    pub fn take_head(&mut self) -> Option<Pattern> {
        self.pats.pop_front()
    }

    pub fn split_head(&mut self, len: usize) -> Vec<Pattern> {
        let deque = self.pats.split_off(len);
        std::mem::replace(&mut self.pats, deque).into()
    }

    pub fn insert_head(&mut self, pat: Pattern) {
        self.pats.push_front(pat);
    }

    pub fn append_head<IntoIter>(&mut self, pats: IntoIter)
    where
        IntoIter: IntoIterator<Item = Pattern>,
        IntoIter::IntoIter: DoubleEndedIterator,
    {
        for p in pats.into_iter().rev() {
            self.pats.push_front(p);
        }
    }

    pub fn specialize(mut self, tag: &Tag) -> Option<Self> {
        if let Some(head) = self.take_head() {
            match head {
                Pattern::Constructor(t, ps) if t == *tag => {
                    self.append_head(ps);
                    Some(self)
                }
                Pattern::Constructor(_, _) => None,
                Pattern::Wildcard => {
                    self.append_head(vec![Pattern::Wildcard; tag.arity()]);
                    Some(self)
                }
            }
        } else {
            None
        }
    }

    pub fn restore_specialized(&mut self, tag: &Tag) {
        let args = self.split_head(tag.arity());
        self.insert_head(Pattern::Constructor(tag.clone(), args));
    }

    pub fn defaults(mut self) -> Option<Self> {
        if let Some(head) = self.take_head() {
            if head.is_wildcard() {
                Some(self)
            } else {
                None
            }
        } else {
            None
        }
    }
}
