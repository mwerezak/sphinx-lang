use rustc_hash::FxHashMap;

pub struct RuntimeObject {
    //type: GCHandle
    table: FxHashMap,
}