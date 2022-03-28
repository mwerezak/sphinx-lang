use rustc_hash::FxHashMap;

pub struct RuntimeObject {
    //type: GC
    table: FxHashMap,
}