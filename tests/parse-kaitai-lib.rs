use std::fs::{read_dir, File};
use std::io::Read;

use kaitai::parse;


#[test]
fn parse_all() {
    let p = format!("{}/tests/ksy-files/", env!("CARGO_MANIFEST_DIR"));

    for ent in read_dir(p).unwrap() {
        eprintln!("{:?}", ent);
        let ent = ent.unwrap();
        let mut fd = File::open(ent.path()).unwrap();
        let mut buf = String::default();

        fd.read_to_string(&mut buf).unwrap();
        parse(&buf)
    }
}
