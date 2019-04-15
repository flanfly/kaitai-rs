use std::sync::atomic::{AtomicUsize, Ordering};
use std::borrow::Cow;

use yaml_rust::{Yaml, YamlLoader};

static NEXT_UNNAMED_ID: AtomicUsize = AtomicUsize::new(0);

enum Endianess {
    Big,
    Little,
}

#[derive(Debug)]
struct Meta<'a> {
    id: Cow<'a, str>,
    /*title: &'a str,
    application: &'a str,
    fileExtension: &'a str,
    xref: &'a str,
    license: &'a str,
    ksVersion: &'a str,
    ksDebug: &'a str,
    ksOpaqueTypes: &'a str,
    imports: &'a str,
    encoding: &'a str,
    endian: &'a str,*/
}

impl<'a> Meta<'a> {
    pub fn parse(yml: &'a Yaml) -> Self {
        let meta = yml.as_hash().unwrap();
        let id: Cow<'a, str> = meta
            .get(&Yaml::from_str("id"))
            .and_then(Yaml::as_str)
            .map(|x| Cow::Borrowed(x))
            .unwrap_or_else(Self::unnamed);

        Meta{ id }
    }

    fn unnamed() -> Cow<'a, str> {
        let i = NEXT_UNNAMED_ID.fetch_add(1, Ordering::Relaxed);
        Cow::Owned(format!("unnamed{}", i))
    }
}

pub fn parse(ksy: &str) {
    match YamlLoader::load_from_str(ksy) {
        Ok(ymls) => for yml in ymls {
            let meta = &yml.as_hash().unwrap()[&Yaml::from_str("meta")];
            eprintln!("{:?}", Meta::parse(meta));
        }
        Err(e) => panic!(e),
    }
}
