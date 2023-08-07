mod utils;

extern crate bitvec;

use bitvec::prelude::*;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct Universe {
    cells: BitVec,
}

#[wasm_bindgen]
impl Universe {
    pub fn new(span: usize) -> Universe {
        let cells = BitVec::repeat(false, span);

        Universe {
            cells,
        }
    }

    pub fn span(&self) -> usize {
        self.cells.len()
    }

    pub fn set(&mut self, index: usize, value: bool) {
        self.cells.set(index, value);
    }

    pub fn count_alive(&self) -> usize {
        self.cells.as_bitslice().count_ones()
    }

    pub fn as_ptr(&self) -> *const usize {
        self.cells.as_bitptr().pointer()
    }

    pub fn iterate(&mut self, rule: &BasicRule, state: &mut EmptyState)
    {
        rule.apply(state, self);
    }
}

trait State {
}

#[wasm_bindgen]
pub struct EmptyState {}

#[wasm_bindgen]
impl EmptyState {
    pub fn new() -> EmptyState {
        EmptyState {}
    }
}

impl State for EmptyState {}

trait Rule<T: State> {
    fn apply(&self, state: &mut T, universe: &mut Universe);
}

#[wasm_bindgen]
pub struct BasicRule {
    rule_index: u64,
    window_size: usize,
}

#[wasm_bindgen]
impl BasicRule {
    pub fn new(rule_index: u64, window_size: usize) -> BasicRule {
        BasicRule {
            rule_index,
            window_size,
        }
    }

    pub fn rule_index(&self) -> u64 {
        self.rule_index
    }

    pub fn window_size(&self) -> usize {
        self.window_size
    }
}

impl Rule<EmptyState> for BasicRule {
    fn apply(&self, _: &mut EmptyState, universe: &mut Universe) {
        let window_mask = (2 as i8).pow(self.window_size as u32) - 1;
        let window_mask = window_mask as u8;

        let offset = universe.span() - self.window_size + 1;

        let mut cache: u8 = universe.cells[offset..]
            .iter()
            .rev()
            .enumerate()
            .map(|(i, val)| {
                println!("building... {:?}", i);
                (*val as u8) << i
            })
            .sum();

        let bits = universe.cells.as_mut_bitslice();
        for mut bit in bits.iter_mut() {
            cache = (*bit as u8) + (cache << 1);
            cache &= window_mask;

            println!("cache: {:?}", cache);

            *bit = self.rule_index & (1 << cache) != 0;
        }

        let shift = (self.window_size - 1) / 2;
        bits.rotate_left(shift);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rule_new() {
        let rule = BasicRule::new(110, 3);

        assert_eq!(rule.rule_index, 110);
        assert_eq!(rule.window_size, 3);
    }

    #[test]
    fn universe_new() {
        let mut universe = Universe::new(10);

        assert_eq!(universe.span(), 10);
        assert_eq!(universe.count_alive(), 0);

        universe.set(5, true);
        assert_eq!(universe.count_alive(), 1);
    }

    #[test]
    fn basic_rule_apply() {
        let mut universe = Universe::new(11);
        universe.set(5, true);

        let mut state = EmptyState::new();
        let rule = BasicRule::new(254, 3);

        rule.apply(&mut state, &mut universe);
        assert_eq!(&universe.cells[..], bits![
            0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0
        ]);
    }

    #[test]
    fn basic_rule_apply_left() {
        let mut universe = Universe::new(11);
        universe.set(0, true);

        let mut state = EmptyState::new();
        let rule = BasicRule::new(254, 3);

        rule.apply(&mut state, &mut universe);
        assert_eq!(&universe.cells[..], bits![
            1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1
        ]);

        rule.apply(&mut state, &mut universe);
        assert_eq!(&universe.cells[..], bits![
            1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1
        ]);
    }

    #[test]
    fn basic_rule_apply_right() {
        let mut universe = Universe::new(11);
        universe.set(10, true);

        let mut state = EmptyState::new();
        let rule = BasicRule::new(254, 3);

        rule.apply(&mut state, &mut universe);
        assert_eq!(&universe.cells[..], bits![
            1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1
        ]);

        rule.apply(&mut state, &mut universe);
        assert_eq!(&universe.cells[..], bits![
            1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1
        ]);
    }
}
