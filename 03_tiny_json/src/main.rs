mod lexer;
mod parser;
mod span;

use std::{collections::BTreeMap, env, fmt::Debug, fs, process};

use parser::Value;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("expected a single argument (the file path)");
        process::exit(1);
    }

    let filename = &args[1];
    let source = fs::read_to_string(filename).unwrap_or_else(|e| {
        eprintln!("Failed to read file: {}", e);
        process::exit(1);
    });

    let (value, errors) = parser::parse_json(&source);
    if !errors.is_empty() {
        for err in &errors {
            eprintln!("Error at {}: {}", err.span, err.message);
        }
        process::exit(1);
    }

    if let Some(v) = value.as_ref() {
        dump(v);
    }
}

struct OrderedOutput<'a>(&'a Value);

impl Debug for OrderedOutput<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Expensive but simple ...
        match self.0 {
            Value::Object(hash) => {
                let ordered: BTreeMap<_, _> =
                    hash.iter().map(|(k, v)| (k, OrderedOutput(v))).collect();
                return ordered.fmt(f);
            }
            Value::Array(array) => {
                let ordered: Vec<_> = array.iter().map(OrderedOutput).collect();
                return ordered.fmt(f);
            }
            v => write!(f, "{:?}", v),
        }
    }
}

fn dump(value: &Value) {
    println!("{:#?}", OrderedOutput(value));
}
