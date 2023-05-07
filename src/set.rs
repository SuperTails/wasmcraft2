use bit_set::BitSet;
use std::{fmt, marker::PhantomData};

#[derive(Debug, Default, PartialEq, Eq, Clone)]
pub struct Set<T> {
    data: BitSet,
    marker: PhantomData<T>,
}

impl<T> Set<T> {
    pub fn new() -> Self {
        Set {
            data: BitSet::new(),
            marker: PhantomData,
        }
    }
}

impl<T: Enumerable> Set<T> {
    pub fn remove(&mut self, elem: T) {
        self.data.remove(elem.to_usize());
    }

    /// Equivalent to calling `remove` on all elements in the iterator.
    pub fn remove_from<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = T>,
    {
        for elem in iter {
            self.remove(elem);
        }
    }

    /// Makes this set the difference with the specified other set in-place.
    pub fn difference_with(&mut self, other: &Self) {
        self.data.difference_with(&other.data);
    }

    pub fn insert(&mut self, elem: T) {
        self.data.insert(elem.to_usize());
    }

    /// Unions in-place with the specified other set.
    pub fn union_with(&mut self, other: &Self) {
        self.data.union_with(&other.data);
    }

    pub fn contains(&self, elem: T) -> bool {
        self.data.contains(elem.to_usize())
    }

    pub fn iter(&self) -> SetIter<T> {
        SetIter(self.data.iter(), PhantomData)
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }
}

impl<T: Enumerable> Extend<T> for Set<T> {
    fn extend<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = T>,
    {
        for elem in iter {
            self.insert(elem);
        }
    }
}

pub struct SetIter<'a, T>(bit_set::Iter<'a, u32>, PhantomData<T>);

impl<T: Enumerable> Iterator for SetIter<'_, T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(T::from_usize)
    }
}

impl<'a, T: Enumerable> IntoIterator for &'a Set<T> {
    type Item = T;

    type IntoIter = SetIter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<T: Enumerable> FromIterator<T> for Set<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut result = Set::new();
        for elem in iter {
            result.insert(elem);
        }
        result
    }
}

/*
struct Cluster {
    start: usize,
    data: BitSet,
}

pub struct ClusteredSet<T> {
    data: Vec<bool>,
    _marker: PhantomData<T>,
}

impl<T> CompactSet<T> {
    pub fn new() -> Self {
        CompactSet {
            start: 0,
            len: 0,
            data: Vec::new(),
            _marker: PhantomData,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn len(&self) -> usize {
        self.len
    }
}

*/

pub trait Enumerable {
    fn to_usize(self) -> usize;

    fn from_usize(index: usize) -> Self;
}

impl Enumerable for usize {
    fn to_usize(self) -> usize {
        self
    }

    fn from_usize(index: usize) -> Self {
        index
    }
}

/*#[derive(Default, Debug, PartialEq, Eq, Clone)]
pub struct PairMap<K, V>(Vec<(K, V)>);

impl<K, V> PairMap<K, V> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn insert()
}*/

/*impl disjoint_sets::ElementType for Temp {
    fn from_usize(n: usize) -> Option<Self> {
        u32::try_from(n).ok().map(Temp)
    }

    fn to_usize(self) -> usize {
        self.0 as usize
    }
}*/

#[derive(PartialEq, Eq, Clone)]
pub struct DenseMap<K, V> {
    values: Vec<Option<V>>,
    marker: PhantomData<K>,
}

impl<K, V> DenseMap<K, V> {
    pub fn new() -> Self {
        DenseMap {
            values: Vec::new(),
            marker: PhantomData,
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        DenseMap {
            values: Vec::with_capacity(capacity),
            marker: PhantomData,
        }
    }
}

impl<K, V> Default for DenseMap<K, V> {
    fn default() -> Self {
        DenseMap::new()
    }
}

impl<K: Enumerable, V> DenseMap<K, V> {
    pub fn insert(&mut self, k: K, v: V) -> Option<V> {
        let k = k.to_usize();
        if k >= self.values.len() {
            self.values.resize_with(k + 1, || None);
        }

        std::mem::replace(&mut self.values[k], Some(v))
    }

    pub fn get(&self, k: K) -> Option<&V> {
        let k = k.to_usize();
        self.values.get(k).and_then(|v| v.as_ref())
    }

    pub fn get_mut(&mut self, k: K) -> Option<&mut V> {
        let k = k.to_usize();
        self.values.get_mut(k).and_then(|v| v.as_mut())
    }

    pub fn contains(&self, k: K) -> bool {
        self.get(k).is_some()
    }

    pub fn iter(&self) -> DenseMapIter<K, V> {
        DenseMapIter(self.values.iter().enumerate(), PhantomData)
    }

    #[allow(clippy::needless_lifetimes)] // false positive
    pub fn keys<'a>(&'a self) -> impl Iterator<Item = K> + 'a {
        self.iter().map(|(k, _)| k)
    }
}

pub struct DenseMapIter<'a, K, V>(
    std::iter::Enumerate<std::slice::Iter<'a, Option<V>>>,
    PhantomData<K>,
);

impl<'a, K: Enumerable, V> Iterator for DenseMapIter<'a, K, V> {
    type Item = (K, &'a V);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let pair = self.0.next()?;
            if let (key, Some(value)) = pair {
                let key = K::from_usize(key);
                return Some((key, value));
            }
        }
    }
}

impl<K: Enumerable + fmt::Debug, V: fmt::Debug> fmt::Debug for DenseMap<K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map().entries(self.iter()).finish()
    }
}

/// Represents all key-value pairs as literal pairs in a Vec.
/// Good for a map with only a small handful of entries and quickly-comparable keys.
#[derive(Clone)]
pub struct PairMap<K, V> {
    values: Vec<(K, V)>,
}

impl<K, V> PairMap<K, V> {
    pub fn new() -> Self {
        PairMap {
            values: Vec::new(),
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        PairMap {
            values: Vec::with_capacity(capacity),
        }
    }

    pub fn len(&self) -> usize {
        self.values.len()
    }

    pub fn is_empty(&self) -> bool {
        self.values.is_empty()
    }
}

impl<K, V> Default for PairMap<K, V> {
    fn default() -> Self {
        PairMap::new()
    }
}

impl<K: PartialEq + Eq, V> PairMap<K, V> {
    pub fn insert(&mut self, k: K, v: V) -> Option<V> {
        let prev = self.values.iter_mut().find(|(key, _)| key == &k);
        if let Some((_, prev_value)) = prev {
            Some(std::mem::replace(prev_value, v))
        } else {
            self.values.push((k, v));
            None
        }
    }

    pub fn remove(&mut self, k: &K) -> Option<V> {
        let prev = self.values.iter_mut().enumerate().find(|(_, (key, _))| key == k);
        if let Some((prev, _)) = prev {
            Some(self.values.swap_remove(prev).1)
        } else {
            None
        }
    }

    pub fn get(&self, k: &K) -> Option<&V> {
        self.values.iter().find(|(key, _)| key == k).map(|(_, v)| v)
    }

    pub fn get_mut(&mut self, k: &K) -> Option<&mut V> {
        self.values.iter_mut().find(|(key, _)| key == k).map(|(_, v)| v)
    }

    pub fn contains_key(&self, k: &K) -> bool {
        self.get(k).is_some()
    }

    pub fn iter(&self) -> impl Iterator<Item = &(K, V)> {
        self.values.iter()
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut (K, V)> {
        self.values.iter_mut()
    }

    #[allow(clippy::needless_lifetimes)] // false positive
    pub fn keys<'a>(&'a self) -> impl Iterator<Item = &'a K> + 'a {
        self.iter().map(|(k, _)| k)
    }
}

impl<K, V> IntoIterator for PairMap<K, V> {
    type Item = (K, V);

    type IntoIter = std::vec::IntoIter<(K, V)>;

    fn into_iter(self) -> Self::IntoIter {
        self.values.into_iter()
    }
}

impl<K: PartialEq + Eq + fmt::Debug, V: fmt::Debug> fmt::Debug for PairMap<K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let i = self.iter().map(|(k, v)| (k, v));
        f.debug_map().entries(i).finish()
    }
}
