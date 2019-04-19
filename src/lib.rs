#[macro_use]
extern crate serde;

use std::borrow::Cow;
use std::fmt;
use std::str::FromStr;
use std::sync::atomic::{AtomicUsize, Ordering};

use charsets::Charset;
use serde::de::{self, SeqAccess, Visitor};
use serde::{Deserialize, Deserializer};
use serde_yaml::Sequence;
use yaml_rust::{Yaml, YamlLoader};

static NEXT_UNNAMED_ID: AtomicUsize = AtomicUsize::new(0);

#[derive(Debug, Deserialize)]
struct Ksy {
    meta: Meta,
    #[serde(default)]
    seq: Sequence,
}

#[derive(Debug)]
enum Endianess {
    Big,
    Little,
}

impl<'de> Deserialize<'de> for Endianess {
    fn deserialize<D>(deserializer: D) -> Result<Endianess, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_str(StringVisitor(deserializeEndianess))
    }
}

struct StringVisitor<T>(fn(String) -> Result<T, String>);

impl<'de, T> Visitor<'de> for StringVisitor<T> {
    type Value = T;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("a string")
    }

    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        self.0(value.to_string()).map_err(|e| de::Error::custom(e))
    }
}

struct StringListVisitor<T>(fn(Vec<String>) -> Result<T, String>);

impl<'de, T> Visitor<'de> for StringListVisitor<T> {
    type Value = T;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("a string")
    }

    fn visit_seq<S>(self, mut seq: S) -> Result<Self::Value, S::Error>
    where
        S: SeqAccess<'de>,
    {
        let mut ret = Vec::with_capacity(seq.size_hint().unwrap_or(1));

        while let Ok(Some(v)) = seq.next_element() {
            ret.push(v)
        }

        self.0(ret).map_err(|e| de::Error::custom(e))
    }

    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        self.0(vec![value.to_string()]).map_err(|e| de::Error::custom(e))
    }
}

fn deserializeEndianess(value: String) -> Result<Endianess, String> {
    if value == "le" {
        Ok(Endianess::Little)
    } else if value == "be" {
        Ok(Endianess::Big)
    } else {
        Err(format!("Unknown endianess {}", value))
    }
}

impl Default for Endianess {
    fn default() -> Endianess {
        Endianess::Little
    }
}

fn defaultFalse() -> bool {
    false
}

fn defaultEncoding() -> Charset {
    Charset::Utf8
}

fn defaultEndianess() -> Endianess {
    Endianess::Little
}

fn defaultFileExtension() -> Vec<String> {
    Vec::default()
}

fn deserializeEncoding<'de, D>(de: D) -> Result<Charset, D::Error>
where
    D: Deserializer<'de>,
{
    de.deserialize_str(StringVisitor(encodingFromStr))
}

fn encodingFromStr(s: String) -> Result<Charset, String> {
    Charset::from_str(&s).map_err(|_| format!("{} is not a valid charset", s))
}

fn deserializeFileExtension<'de, D>(de: D) -> Result<Vec<String>, D::Error>
where
    D: Deserializer<'de>,
{
    de.deserialize_any(StringListVisitor(identityFromStrList))
}

fn identityFromStrList(s: Vec<String>) -> Result<Vec<String>, String> {
    Ok(s)
}

fn deserializeApplication<'de, D>(de: D) -> Result<Vec<String>, D::Error>
where
    D: Deserializer<'de>,
{
    de.deserialize_any(StringListVisitor(identityFromStrList))
}

#[derive(Debug, Deserialize)]
struct Meta {
    /// Identifier for a primary structure described in top-level map
    id: String,
    /// Optional free-form text string that is a longer title of this .ksy file
    #[serde(default)]
    title: String,
    #[serde(default)]
    #[serde(deserialize_with = "deserializeApplication")]
    application: Vec<String>,
    #[serde(default)]
    import: Vec<String>,
    #[serde(deserialize_with = "deserializeEncoding")]
    #[serde(default = "defaultEncoding")]
    encoding: Charset,
    #[serde(default)]
    endian: Endianess,
    #[serde(default)]
    ks_version: String,
    #[serde(default = "defaultFalse")]
    ks_debug: bool,
    #[serde(default = "defaultFalse")]
    ks_opaque_types: bool,
    #[serde(default)]
    license: String,
    #[serde(deserialize_with = "deserializeFileExtension")]
    #[serde(default = "defaultFileExtension")]
    fileExtension: Vec<String>,
}
/*
impl<'a> Meta<'a> {
    pub fn parse(yml: &'a Yaml) -> Self {
        let meta = yml.as_hash().unwrap();
        let id: Cow<'a, str> = meta
            .get(&Yaml::from_str("id"))
            .and_then(Yaml::as_str)
            .map(|x| Cow::Borrowed(x))
            .unwrap_or_else(Self::unnamed);

        Meta { id }
    }

    fn unnamed() -> Cow<'a, str> {
        let i = NEXT_UNNAMED_ID.fetch_add(1, Ordering::Relaxed);
        Cow::Owned(format!("unnamed{}", i))
    }
}
*/

pub fn parse(ksy: &str) {
    let ksy: Ksy = serde_yaml::from_str(ksy).unwrap();
    /*
    match YamlLoader::load_from_str(ksy) {
        Ok(ymls) => {
            for yml in ymls {
                let meta = &yml.as_hash().unwrap()[&Yaml::from_str("meta")];
                eprintln!("{:?}", Meta::parse(meta));
            }
        }
        Err(e) => panic!(e),
    }
    */
}
