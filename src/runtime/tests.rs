#![cfg(test)]

use std::hash::{Hasher, BuildHasher};
use crate::runtime::strings::FxHashBuilder;

use std::hash::BuildHasherDefault;
use std::collections::hash_map::DefaultHasher;


#[test]
fn hash_intern_hasher_is_stable() {
    let string = "vjiowjtajfioaj3r=3-ovp0-sikf";
    
    let hasher_factory = FxHashBuilder::default();
    // let hasher_factory = BuildHasherDefault::<DefaultHasher>::default();
    
    let mut last_hash = None;
    
    for _i in 0..10 {
        let mut hasher = hasher_factory.build_hasher();
        
        hasher.write(string.as_bytes());
        let hash = hasher.finish();
        
        println!("hash: {}", hash);
        if let Some(prev) = last_hash.replace(hash) {
            assert!(hash == prev);
        }
    }
    
    
}