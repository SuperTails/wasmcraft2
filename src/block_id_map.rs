use crate::ssa::BlockId;


/// Only allows block IDs from within a single function.
///
/// Stores values as a Vec<Option<T>>, because BlockIds are usually densely packed.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LocalBlockMap<T> {
	func_id: Option<usize>,
	data: Vec<Option<T>>,
}

impl<T> LocalBlockMap<T> {
	pub fn new(func_id: usize) -> Self {
		LocalBlockMap { func_id: Some(func_id), data: Vec::new() }
	}

	pub fn func_id(&self) -> usize {
		self.func_id.unwrap()
	}

	pub fn len(&self) -> usize {
		self.data.iter().filter(|d| d.is_some()).count()
	}

	pub fn is_empty(&self) -> bool {
		self.len() == 0
	}

	fn check_func_id(&mut self, func_id: usize) {
		if let Some(id) = self.func_id {
			assert_eq!(id, func_id);
		} else {
			self.func_id = Some(func_id);
		}
	}

	pub fn insert(&mut self, k: BlockId, v: T) -> Option<T> {
		self.check_func_id(k.func);

		if k.block >= self.data.len() {
			self.data.resize_with(k.block + 1, Default::default);
		}

		std::mem::replace(&mut self.data[k.block], Some(v))
	}

	pub fn get(&self, k: BlockId) -> Option<&T> {
		if let Some(f) = self.func_id { assert_eq!(k.func, f); }

		self.data.get(k.block).and_then(|v| v.as_ref())
	}

	pub fn get_mut(&mut self, k: BlockId) -> Option<&mut T> {
		if let Some(f) = self.func_id { assert_eq!(k.func, f); }

		self.data.get_mut(k.block).and_then(|v| v.as_mut())
	}

	pub fn remove(&mut self, k: BlockId) -> Option<T> {
		if let Some(f) = self.func_id { assert_eq!(k.func, f); }

		if let Some(entry) = self.data.get_mut(k.block) {
			entry.take()
		} else {
			None
		}
	}

	pub fn entry(&mut self, k: BlockId) -> &mut Option<T> {
		if let Some(f) = self.func_id { assert_eq!(k.func, f); }

		if k.block >= self.data.len() {
			self.data.resize_with(k.block + 1, Default::default);
		}

		&mut self.data[k.block]
	}

	pub fn contains_key(&self, k: BlockId) -> bool {
		self.get(k).is_some()
	}

	pub fn iter(&self) -> impl Iterator<Item=(BlockId, &T)> + '_ {
		let func = self.func_id.unwrap_or(usize::MAX);

		self.data.iter().enumerate().flat_map(move |(idx, v)| {
			let block_id = BlockId { func, block: idx };
			v.as_ref().map(|v| (block_id, v))
		})
	}

	pub fn iter_mut(&mut self) -> impl Iterator<Item=(BlockId, &mut T)> {
		let func = self.func_id.unwrap_or(usize::MAX);

		self.data.iter_mut().enumerate().flat_map(move |(idx, v)| {
			let block_id = BlockId { func, block: idx };
			v.as_mut().map(|v| (block_id, v))
		})
	}

	pub fn keys(&self) -> impl Iterator<Item=BlockId> + '_ {
		self.iter().map(|(k, _v)| k)
	}

	pub fn values(&self) -> impl Iterator<Item=&T> {
		self.iter().map(|(_k, v)| v)
	}

	/// Includes keys that don't have a corresponding value in the map.
	pub fn iter_all_keys(&self) -> impl Iterator<Item=BlockId> {
		let func = self.func_id.unwrap();

		(0..self.data.len()).map(move |i| {
			BlockId { func, block: i }
		})
	}
}

pub struct LocalBlockMapIntoIter<T>(LocalBlockMap<T>, usize);

impl<V> Iterator for LocalBlockMapIntoIter<V> {
    type Item = (BlockId, V);

    fn next(&mut self) -> Option<Self::Item> {
		while self.1 < self.0.data.len() {
			let old_idx = self.1;
			self.1 += 1;

			if let Some(value) = self.0.data[old_idx].take() {
				return Some((BlockId { func: self.0.func_id.unwrap(), block: old_idx }, value))
			}
		}

		None
    }
}

impl<V> IntoIterator for LocalBlockMap<V> {
    type Item = (BlockId, V);

    type IntoIter = LocalBlockMapIntoIter<V>;

    fn into_iter(self) -> Self::IntoIter {
		LocalBlockMapIntoIter(self, 0)
    }
}

impl<V> FromIterator<(BlockId, V)> for LocalBlockMap<V> {
    fn from_iter<T: IntoIterator<Item = (BlockId, V)>>(iter: T) -> Self {
		let mut data = Vec::new();

		let mut func_id = None;

		for (block_id, block) in iter.into_iter() {
			if let Some(func_id) = func_id {
				assert_eq!(func_id, block_id.func);
			} else {
				func_id = Some(block_id.func);
			}

			if block_id.block >= data.len() {
				data.resize_with(block_id.block + 1, || None);
			}

			assert!(data[block_id.block].is_none());
			data[block_id.block] = Some(block);
		}

		LocalBlockMap { func_id, data }
    }
}