use smallvec::{smallvec, SmallVec};
use std::cell::RefCell;
use std::collections::BTreeMap;

// TODO: Probably inefficient, replace with an algorithm to calculate SCC

/// An enumeration of the dependencies of a value. Used in topological sorting.
pub trait DependencyList<K> {
    fn traverse_dependencies(&self, f: &mut impl FnMut(&K));
}

impl<'a, T: DependencyList<K>, K> DependencyList<K> for &'a T {
    fn traverse_dependencies(&self, f: &mut impl FnMut(&K)) {
        T::traverse_dependencies(self, f)
    }
}

/// Perform a topological sort.
pub fn topological_sort<K: Ord, V: DependencyList<K>>(
    data: impl IntoIterator<Item = (K, V)>,
) -> Vec<SmallVec<[V; 1]>> {
    let state = TopologicalSort {
        map: data
            .into_iter()
            .map(|(k, v)| (k, RefCell::new(Marked::None(v))))
            .collect(),
    };
    let mut result = Vec::new();
    for key in state.map.keys() {
        state.visit(key, 0, &mut result);
    }
    result
}

type Depth = u32;

enum Marked<V> {
    None(V),
    Temporary(Depth, V),
    Permanent,
}

impl<V> Marked<V> {
    fn mark_as_temporary(&mut self, depth: Depth) {
        match self {
            Self::None(v) => unsafe {
                let v = std::ptr::read(v);
                std::ptr::write(self, Self::Temporary(depth, v));
            },
            _ => panic!("mark_as_temporary must be called on Marked::None"),
        }
    }

    fn mark_as_permanent(&mut self) -> V {
        match self {
            Self::Temporary(_, v) => unsafe {
                let v = std::ptr::read(v);
                std::ptr::write(self, Self::Permanent);
                v
            },
            _ => panic!("mark_as_permanent must be called on Marked::Temporary"),
        }
    }
}

enum Cycle<V> {
    Acyclic,
    Cyclic(Depth, SmallVec<[V; 1]>),
}

impl<V> Cycle<V> {
    fn merge(&mut self, other: Cycle<V>) {
        match self {
            Cycle::Acyclic => *self = other,
            Cycle::Cyclic(depth, vs) => match other {
                Cycle::Acyclic => {}
                Cycle::Cyclic(d, mut v) => {
                    *depth = std::cmp::min(*depth, d);
                    vs.append(&mut v);
                }
            },
        }
    }
}

impl<K, V: DependencyList<K>> DependencyList<K> for Marked<V> {
    fn traverse_dependencies(&self, f: &mut impl FnMut(&K)) {
        match self {
            Marked::Temporary(_, v) => v.traverse_dependencies(f),
            _ => panic!("dependency_list"),
        }
    }
}

struct TopologicalSort<K, V> {
    map: BTreeMap<K, RefCell<Marked<V>>>,
}

impl<K: Ord, V: DependencyList<K>> TopologicalSort<K, V> {
    fn visit(&self, key: &K, depth: Depth, result: &mut Vec<SmallVec<[V; 1]>>) -> Cycle<V> {
        let cell = match self.map.get(key) {
            Some(cell) => cell,
            None => return Cycle::Acyclic,
        };

        match *cell.borrow() {
            Marked::None(_) => {}
            Marked::Temporary(d, _) => return Cycle::Cyclic(d, SmallVec::new()),
            Marked::Permanent => return Cycle::Acyclic,
        }

        cell.borrow_mut().mark_as_temporary(depth);

        let mut cycle = Cycle::Acyclic;
        cell.borrow().traverse_dependencies(&mut |key| {
            cycle.merge(self.visit(key, depth + 1, result));
        });

        let value = cell.borrow_mut().mark_as_permanent();

        match cycle {
            Cycle::Acyclic => {
                result.push(smallvec![value]);
                Cycle::Acyclic
            }
            Cycle::Cyclic(d, mut vs) => {
                vs.push(value);
                if d < depth {
                    Cycle::Cyclic(d, vs)
                } else {
                    result.push(vs);
                    Cycle::Acyclic
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use itertools::Itertools;

    #[derive(Debug, Clone)]
    struct Node {
        name: i32,
        deps: Vec<i32>,
    }

    impl DependencyList<i32> for Node {
        fn traverse_dependencies(&self, f: &mut impl FnMut(&i32)) {
            self.deps.iter().for_each(f);
        }
    }

    fn random_node(n: &(i32, &[i32])) -> impl Iterator<Item = Node> {
        let name = n.0;
        n.1.to_owned()
            .into_iter()
            .permutations(n.1.len())
            .map(move |deps| Node { name, deps })
    }

    fn random_graph(ns: &[(i32, &[i32])]) -> Vec<Vec<Node>> {
        match ns {
            [] => Vec::new(),
            [n] => vec![random_node(n).collect()],
            [n, ns @ ..] => random_node(n)
                .flat_map(|item| {
                    random_graph(ns).into_iter().map(move |mut items| {
                        items.insert(0, item.clone());
                        items
                    })
                })
                .collect(),
        }
    }

    fn run_sort(items: Vec<Node>) -> String {
        let result = topological_sort(items.into_iter().map(|node| (node.name, node)));
        result
            .into_iter()
            .map(|nodes| {
                let mut ls = nodes
                    .into_iter()
                    .map(|n| n.name.to_string())
                    .collect::<Vec<_>>();
                ls.sort();
                ls.join(", ")
            })
            .join(" / ")
    }

    macro_rules! assert_sort {
        ($graph:expr, $($result:pat)|*) => {
            for g in random_graph(&$graph) {
                let result = run_sort(g);
                assert!(matches!(result.as_str(), $($result)|*), "{}", result);
            }
        };
    }

    #[test]
    fn test_topological_sort() {
        assert_sort!([], "");

        assert_sort!([(0, &[])], "0");
        assert_sort!([(0, &[0])], "0");

        assert_sort!([(0, &[1]), (1, &[])], "1 / 0");
        assert_sort!([(0, &[]), (1, &[0])], "0 / 1");
        assert_sort!([(0, &[1]), (1, &[1])], "1 / 0");
        assert_sort!([(0, &[0]), (1, &[0])], "0 / 1");
        assert_sort!([(0, &[1]), (1, &[0])], "0, 1");

        assert_sort!([(0, &[]), (1, &[]), (2, &[])], "0 / 1 / 2");
        assert_sort!([(0, &[2]), (1, &[]), (2, &[1])], "1 / 2 / 0");

        assert_sort!(
            [(0, &[2, 3]), (1, &[0, 2]), (2, &[3]), (3, &[])],
            "3 / 2 / 0 / 1"
        );
        assert_sort!(
            [(0, &[1, 2, 3]), (1, &[2]), (2, &[]), (3, &[])],
            "3 / 2 / 1 / 0" | "2 / 1 / 3 / 0" | "2 / 3 / 1 / 0"
        );
        assert_sort!(
            [(0, &[]), (1, &[2]), (2, &[3]), (3, &[2, 0])],
            "0 / 2, 3 / 1"
        );

        assert_sort!(
            [(0, &[1]), (1, &[2]), (2, &[3, 4]), (3, &[1]), (4, &[0])],
            "0, 1, 2, 3, 4"
        );
        assert_sort!(
            [
                (0, &[]),
                (1, &[0, 4]),
                (2, &[1, 4]),
                (3, &[1, 2, 4]),
                (4, &[])
            ],
            "4 / 0 / 1 / 3 / 2" | "0 / 4 / 1 / 2 / 3"
        );

        assert_sort!(
            [
                (0, &[1, 2]),
                (1, &[2, 3, 4]),
                (2, &[1, 4, 5]),
                (3, &[4]),
                (4, &[5]),
                (5, &[3])
            ],
            "3, 4, 5 / 1, 2 / 0"
        );

        assert_sort!(
            [
                (0, &[1, 4]),
                (1, &[2, 3, 7]),
                (2, &[0, 6]),
                (3, &[4]),
                (4, &[5]),
                (5, &[3]),
                (6, &[7]),
                (7, &[8]),
                (8, &[6])
            ],
            "3, 4, 5 / 6, 7, 8 / 0, 1, 2" | "6, 7, 8 / 3, 4, 5 / 0, 1, 2"
        );

        assert_sort!([(0, &[1])], "0");
    }
}
