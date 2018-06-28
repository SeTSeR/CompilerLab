use frontend::AST;
use frontend::Monoid;
use frontend::Node;
use frontend::Link;

use std::collections::HashSet;
use std::collections::HashMap;
use std::mem;

impl Monoid for HashSet<Number> {
    fn mempty() -> Self {
        HashSet::new()
    }

    fn mappend(&mut self, b: Self) -> () {
        b.iter().for_each(|item| {
            self.insert(item.clone());
        })
    }
}

#[derive(Hash, Eq, PartialEq, Clone)]
struct Number((u64, i16, i8));

impl Number {
    fn new(val: f64) -> Number {
        Number(integer_decode(val))
    }

    fn to_number(&self) -> f64 {
        let Number((mantissa, exponent, sign)) = *self;
        (sign as f64) * (mantissa as f64) * (2f64.powf(exponent as f64))
    }
}

fn integer_decode(val: f64) -> (u64, i16, i8) {
    let bits: u64 = unsafe { mem::transmute(val) };
    let sign: i8 = if bits >> 63 == 0 { 1 } else { -1 };
    let mut exponent: i16 = ((bits >> 52) & 0x7ff) as i16;
    let mantissa = if exponent == 0 {
        (bits & 0xfffffffffffff) << 1
    } else {
        (bits & 0xfffffffffffff) | 0x10000000000000
    };

    exponent -= 1023 + 52;
    (mantissa, exponent, sign)
}

pub fn gen_code(a: f64, b: f64, functions: Vec<AST>, derivatives: Vec<AST>) -> String {
    let collect_node = |node: &Link<Node>| {
        let mut table = HashSet::new();
        match *(*node.borrow()) {
            Node::Number(val) => table.insert(Number::new(val)),
            _ => false,
        };
        table
    };
    let identifiers = functions
        .iter()
        .zip(derivatives.iter())
        .fold(HashSet::new(), |mut acc, (tree, deriv)| {
            let table1 = tree.collect_info(&collect_node);
            let table2 = deriv.collect_info(&collect_node);
            acc.mappend(table1);
            acc.mappend(table2);
            acc
        });
    let identcount = identifiers.len();
    let identifiers: HashMap<Number, String> = identifiers
        .into_iter()
        .zip((1..(identcount + 1)).map(|num|format!("const{}", num)))
        .collect();
    format!("{}\n{}\n{}", gen_header(functions.len(), derivatives.len()),
    gen_rodata(&identifiers, a, b), gen_text(&identifiers, a, b))
}

fn gen_header(funccount: usize, derivcount: usize) -> String {
    let mut strings: Vec<String> = vec!["BITS 64", "default rel", " ", "global a", "global b"]
        .into_iter()
        .map(ToString::to_string)
        .collect();
    strings.append(&mut (1..(funccount + 1))
        .map(|number| format!("global f{}", number))
        .collect());
    strings.append(&mut (1..(funccount + 1))
        .map(|number| format!("global df{}", number))
        .collect());
    strings.join("\n")
}

fn gen_rodata(table: &HashMap<Number, String>, a: f64, b: f64) -> String {
    let elems = table.keys().zip(table.values());
    let mut strings: Vec<String> = vec!["section .rodata".to_string()
    , format!("    a dq {}", a)
    , format!("    b dq {}", b)];
    strings.append(&mut elems.map(|(key, value)| {
        format!("    {} dq {}", value, key.to_number())
    }).collect());
    strings.join("\n")
}

fn gen_text(table: &HashMap<Number, String>, a: f64, b: f64) -> String {
    "".to_string()
}