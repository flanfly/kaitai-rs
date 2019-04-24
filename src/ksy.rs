use std::fmt;
use std::str::FromStr;

use charsets::Charset;
use regex::Regex;
use serde::de::{self, SeqAccess, Visitor};
use serde::{Deserialize, Deserializer};
use serde_yaml::{Mapping, Sequence, Value};

use crate::expr::{self, Expr};

#[derive(Debug)]
pub struct Terminator {
    terminator: u8,
    consume: bool,
    include: bool,
}

#[derive(Debug)]
pub enum Type {
    Integer {
        bytes: usize,
        endian: Endianess,
        enu: Option<String>,
        signed: bool,
    },
    Bits {
        bits: usize,
        contents: Option<Box<[u8]>>,
    },
    String {
        size: Option<Expr>,
        encoding: Charset,
        eos_error: bool,
        terminator: Option<Terminator>,
    },
    Float {
        large: bool,
        endian: Endianess,
    },
    Bytes {
        size: Option<Expr>,
        eos_error: bool,
        terminator: Option<Terminator>,
        process: Option<String>,
    },
    Custom {
        name: String,
    },
    Switch {
        pivot: Expr,
        options: Vec<(Expr, Expr)>,
    },
}

#[derive(Debug)]
pub enum Repeat {
    Eos,
    Until(Expr),
    Fixed(Expr),
}

#[derive(Debug)]
pub enum Instance {
    Computed { value: Expr },
    Positioned { position: Expr, io: Option<Expr> },
}

#[derive(Debug)]
pub struct Attribute {
    id: String,
    repeat: Option<Repeat>,
    if_expr: Option<Expr>,
    doc: Option<String>,
    doc_xref: Option<String>,
    contents: Option<Box<[u8]>>,
    typ: Type,
    instance: Option<Instance>,
}

impl Attribute {
    pub fn newSeq(map: &Mapping, endian: Endianess, charset: Charset) -> Result<Attribute, String> {
        let mut attr = Self::new(map, endian, charset)?;

        attr.id = match map.get(&"id".to_string().into()).map(Value::as_str) {
            Some(Some(x)) => x.to_string(),
            Some(None) => {
                return Err(format!("attribute id is not a string"));
            }
            None => {
                return Err(format!("no id field in type definition"));
            }
        };

        Ok(attr)
    }

    pub fn newInstance(id: String, map: &Mapping, endian: Endianess, charset: Charset) -> Result<Attribute, String> {
        let value = map.get(&"value".to_string().into()).and_then(|x| Expr::from_yaml(x).ok());
        let io = map.get(&"io".to_string().into()).and_then(|x| Expr::from_yaml(x).ok());
        let pos = map.get(&"pos".to_string().into()).and_then(|x| Expr::from_yaml(x).ok());

        match (pos, value) {
            (Some(pos), None) => {
                let mut attr = Self::new(map, endian, charset)?;

                attr.id = id;
                attr.instance = Some(Instance::Positioned { position: pos, io: io });
                Ok(attr)
            }
            (None, Some(value)) => Ok(Attribute {
                id: id,
                repeat: None,
                if_expr: None,
                doc: None,
                doc_xref: None,
                contents: None,
                typ: Type::String {
                    size: None,
                    eos_error: true,
                    encoding: charset,
                    terminator: None,
                },
                instance: Some(Instance::Computed { value: value }),
            }),
            (Some(_), Some(_)) => Err(format!("both pos and value specified for instance")),
            (None, None) => Err(format!("neither pos nor value specified for instance")),
        }
    }

    fn new(map: &Mapping, endian: Endianess, charset: Charset) -> Result<Attribute, String> {
        let typ = map.get(&"type".to_string().into());
        let int_pat = Regex::new(r"^(u|s)(1|2|4|8)(le|be)?$").unwrap();
        let float_pat = Regex::new(r"^f(4|8)(le|be)?$").unwrap();
        let repeat = match map.get(&"repeat".to_string().into()) {
            Some(x) if x == "eos" => Some(Repeat::Eos),
            Some(x) if x == "until" => {
                let until = map
                    .get(&"repeat-until".to_string().into())
                    .ok_or(format!("missing repeat-until field"))
                    .and_then(Expr::from_yaml)?;

                Some(Repeat::Until(until))
            }
            Some(x) if x == "expr" => {
                let ex = map
                    .get(&"repeat-expr".to_string().into())
                    .ok_or(format!("missing repeat-expr field"))
                    .and_then(Expr::from_yaml)?;

                Some(Repeat::Fixed(ex))
            }
            Some(x) => {
                return Err(format!("illegal repeat value {:?}", x));
            }
            None => None,
        };
        let if_expr = map.get(&"if".to_string().into()).and_then(|x| Expr::from_yaml(x).ok());
        let doc = map.get(&"doc".to_string().into()).and_then(Value::as_str).map(str::to_string);
        let doc_xref = map.get(&"doc-xref".to_string().into()).and_then(Value::as_str).map(str::to_string);
        let contents = match map.get(&"contents".to_string().into()) {
            Some(Value::String(s)) => Some(Vec::from(s.as_bytes()).into_boxed_slice()),
            Some(Value::Sequence(seq)) => {
                let mut ret = Vec::with_capacity(seq.len());

                for v in seq {
                    match v {
                        Value::String(s) => ret.extend(s.bytes()),
                        Value::Number(n) if n.is_i64() => ret.push(n.as_i64().unwrap() as i8 as u8),
                        Value::Number(n) if n.is_u64() => ret.push(n.as_u64().unwrap() as u8),
                        _ => {
                            return Err(format!("illegal contents value {:?}", v));
                        }
                    }
                }

                Some(ret.into_boxed_slice())
            }
            None => None,
            Some(v) => {
                return Err(format!("illegal contents value {:?}", v));
            }
        };

        let typ = match typ {
            // byte array
            None => {
                let size = map
                    .get(&"size".to_string().into())
                    .ok_or(format!("illegal size field value"))
                    .and_then(Expr::from_yaml);
                let eos = map.get(&"size-eos".to_string().into()).and_then(Value::as_bool).unwrap_or(false);
                let eos_err = map.get(&"eos-error".to_string().into()).and_then(Value::as_bool).unwrap_or(true);
                let process = map.get(&"process".to_string().into()).and_then(Value::as_str).map(str::to_string);
                let term = map.get(&"terminator".to_string().into()).and_then(Value::as_u64);
                let consume = map.get(&"consume".to_string().into()).and_then(Value::as_bool).unwrap_or(false);
                let include = map.get(&"include".to_string().into()).and_then(Value::as_bool).unwrap_or(false);

                if size.is_ok() {
                    Type::Bytes {
                        size: size.ok(),
                        eos_error: eos_err,
                        terminator: None,
                        process: process,
                    }
                } else if eos {
                    Type::Bytes {
                        size: None,
                        eos_error: eos_err,
                        terminator: None,
                        process: process,
                    }
                } else if term.is_some() {
                    Type::Bytes {
                        size: None,
                        eos_error: eos_err,
                        terminator: Some(Terminator {
                            terminator: term.unwrap() as u8,
                            consume: consume,
                            include: include,
                        }),
                        process: process,
                    }
                } else if contents.is_some() {
                    Type::Bytes {
                        size: Some(Expr::Value(expr::Value::Unsigned(contents.as_ref().unwrap().len() as u64))),
                        eos_error: eos_err,
                        terminator: None,
                        process: process,
                    }
                } else {
                    return Err(format!("illegal string type spec"));
                }
            }
            // integer
            Some(Value::String(s)) if int_pat.is_match(s) => {
                let caps = int_pat.captures(s).unwrap();
                let signed = caps.get(1).unwrap().as_str() == "s";
                let len = usize::from_str(caps.get(2).unwrap().as_str()).unwrap();
                let endian = match caps.get(3) {
                    Some(m) if m.as_str() == "le" => Endianess::Little,
                    Some(_) => Endianess::Big,
                    None => endian,
                };
                let enu = map.get(&"enum".to_string().into()).and_then(Value::as_str).map(str::to_string);

                Type::Integer {
                    bytes: len,
                    endian: endian,
                    signed: signed,
                    enu: enu,
                }
            }
            // float
            Some(Value::String(s)) if float_pat.is_match(s) => {
                let caps = float_pat.captures(s).unwrap();
                let large = caps.get(1).unwrap().as_str() == "8";
                let endian = match caps.get(2) {
                    Some(m) if m.as_str() == "le" => Endianess::Little,
                    Some(_) => Endianess::Big,
                    None => endian,
                };

                Type::Float { large: large, endian: endian }
            }
            // string
            Some(Value::String(s)) if s == "str" || s == "strz" => {
                let size = map
                    .get(&"size".to_string().into())
                    .ok_or(format!("illegal size field value"))
                    .and_then(Expr::from_yaml);
                let eos = map.get(&"size-eos".to_string().into()).and_then(Value::as_bool).unwrap_or(false);
                let eos_err = map.get(&"eos-error".to_string().into()).and_then(Value::as_bool).unwrap_or(true);
                let enc = map
                    .get(&"encoding".to_string().into())
                    .and_then(Value::as_str)
                    .and_then(|x| Charset::from_str(x).ok())
                    .unwrap_or(charset);
                let term = map.get(&"terminator".to_string().into()).and_then(Value::as_u64);
                let consume = map.get(&"consume".to_string().into()).and_then(Value::as_bool).unwrap_or(false);
                let include = map.get(&"include".to_string().into()).and_then(Value::as_bool).unwrap_or(false);

                if size.is_ok() {
                    Type::String {
                        size: size.ok(),
                        eos_error: eos_err,
                        encoding: enc,
                        terminator: None,
                    }
                } else if eos {
                    Type::String {
                        size: None,
                        eos_error: eos_err,
                        encoding: enc,
                        terminator: None,
                    }
                } else if term.is_some() {
                    Type::String {
                        size: None,
                        eos_error: eos_err,
                        encoding: enc,
                        terminator: Some(Terminator {
                            terminator: term.unwrap() as u8,
                            consume: consume,
                            include: include,
                        }),
                    }
                } else if contents.is_some() {
                    Type::String {
                        size: Some(Expr::Value(expr::Value::Unsigned(contents.as_ref().unwrap().len() as u64))),
                        eos_error: eos_err,
                        encoding: enc,
                        terminator: None,
                    }
                } else {
                    return Err(format!("illegal string type spec"));
                }
            }
            // custom
            Some(Value::String(n)) => Type::Custom { name: n.to_string() },
            // switch
            Some(Value::Mapping(m)) => {
                let pivot = m
                    .get(&"switch-on".to_string().into())
                    .ok_or(format!("missing switch-on field"))
                    .and_then(Expr::from_yaml)?;
                let opts = m
                    .get(&"cases".to_string().into())
                    .and_then(Value::as_mapping)
                    .ok_or(format!("missing cases field"))?;
                let mut m = Vec::<_>::with_capacity(opts.len());

                for (k, v) in opts {
                    let key = Expr::from_yaml(k)?;
                    let value = Expr::from_yaml(v)?;

                    m.push((key, value));
                }

                Type::Switch { pivot: pivot, options: m }
            }
            Some(_) => {
                return Err(format!("illegal type identifier {:?}", typ));
            }
        };

        Ok(Attribute {
            id: "".to_string(),
            contents: contents,
            doc: doc,
            doc_xref: doc_xref,
            if_expr: if_expr,
            repeat: repeat,
            typ: typ,
            instance: None,
        })
    }
}

#[derive(Debug, Deserialize)]
pub struct Ksy {
    meta: Meta,
    #[serde(default)]
    seq: Sequence,
    #[serde(default)]
    doc: String,
    #[serde(default)]
    doc_ref: String,
    #[serde(default)]
    types: Mapping,
    #[serde(default)]
    instances: Mapping,
    #[serde(default)]
    enums: Mapping,
}

#[derive(Debug, Clone, Copy)]
pub enum Endianess {
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

pub fn parse(ksy: &str) {
    let ksy: Ksy = serde_yaml::from_str(ksy).unwrap();

    for (i, val) in ksy.seq.into_iter().enumerate() {
        match Attribute::newSeq(val.as_mapping().unwrap(), ksy.meta.endian, ksy.meta.encoding.clone()) {
            Ok(attr) => eprintln!("{:?}", attr),
            Err(s) => panic!("failed to parse {}th attribute: {}", i, s),
        }
    }

    for (key, val) in ksy.instances {
        let key = key.as_str().unwrap().to_string();

        match Attribute::newInstance(key.clone(), val.as_mapping().unwrap(), ksy.meta.endian, ksy.meta.encoding.clone()) {
            Ok(attr) => eprintln!("{:?}", attr),
            Err(s) => panic!("failed to parse {} instance: {}", key, s),
        }
    }
}
