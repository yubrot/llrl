use smallvec::SmallVec;
use std::collections::BTreeMap;

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
pub fn run<K: Ord, V: DependencyList<K>>(
    data: impl IntoIterator<Item = (K, V)>,
) -> Vec<SmallVec<[V; 1]>> {
    let data = data.into_iter();
    let size = data.size_hint().0;

    let mut state = State {
        keys: BTreeMap::new(),
        indices: Vec::with_capacity(size),
        lowlinks: Vec::with_capacity(size),
        scc_indices: Vec::with_capacity(size),
        next_index: 0,
        next_scc_index: 0,
        stack: Vec::new(),
    };
    let mut values = Vec::with_capacity(size);

    for (i, (k, v)) in data.enumerate() {
        state.keys.insert(k, i);
        values.push(v);
        state.indices.push(-1);
        state.lowlinks.push(-1);
        state.scc_indices.push(-1);
    }
    let size = state.keys.len();
    for i in 0..size {
        if state.indices[i] < 0 {
            state.visit(i, &values);
        }
    }

    let scc_size = state.next_scc_index as usize;
    let mut result = Vec::with_capacity(scc_size);
    result.resize_with(scc_size, SmallVec::new);

    for (value, scc_index) in values.into_iter().zip(state.scc_indices) {
        result[scc_index as usize].push(value);
    }

    result
}

struct State<K> {
    keys: BTreeMap<K, usize>,
    indices: Vec<isize>,
    lowlinks: Vec<isize>,
    scc_indices: Vec<isize>,
    next_index: isize,
    next_scc_index: isize,
    stack: Vec<usize>,
}

impl<K: Ord> State<K> {
    fn visit<V: DependencyList<K>>(&mut self, i: usize, values: &Vec<V>) {
        self.indices[i] = self.next_index;
        self.lowlinks[i] = self.next_index;
        self.stack.push(i);
        self.next_index += 1;

        values[i].traverse_dependencies(&mut |k| {
            if let Some(j) = self.keys.get(k).copied() {
                if self.indices[j] < 0 {
                    self.visit(j, values);
                    self.lowlinks[i] = self.lowlinks[i].min(self.lowlinks[j]);
                } else if self.scc_indices[j] < 0 {
                    self.lowlinks[i] = self.lowlinks[i].min(self.indices[j]);
                }
            }
        });

        if self.indices[i] == self.lowlinks[i] {
            while {
                let j = self.stack.pop().unwrap();
                self.scc_indices[j] = self.next_scc_index;
                j != i
            } {}
            self.next_scc_index += 1;
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
            [] => vec![Vec::new()],
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
        let result = run(items.into_iter().map(|node| (node.name, node)));
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
    fn test_run() {
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
