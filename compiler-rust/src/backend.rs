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
        self.extend(b.into_iter())
    }
}

impl Monoid for Vec<String> {
    fn mempty() -> Self {
        Vec::new()
    }

    fn mappend(&mut self, b: Self) -> () {
        self.extend(b.into_iter())
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
    format!("{}\n\n{}\n\n{}\n", gen_header(functions.len(), derivatives.len()),
    gen_rodata(&identifiers, a, b),
            gen_text(&identifiers, functions.as_slice(), derivatives.as_slice()))
}

fn collect_node(node: &Link<Node>) -> HashSet<Number> {
    let mut table = HashSet::new();
    match *(*node.borrow()) {
        Node::Number(val) => table.insert(Number::new(val)),
        _ => false,
    };
    table
}

fn gen_header(funccount: usize, derivcount: usize) -> String {
    let mut strings: Vec<String> = vec!["[BITS 64]", "default rel", " ", "global a", "global b"]
        .into_iter()
        .map(ToString::to_string)
        .collect();
    strings.append(&mut (1..(funccount + 1))
        .map(|number| format!("global f{}", number))
        .collect());
    strings.append(&mut (1..(derivcount + 1))
        .map(|number| format!("global df{}", number))
        .collect());
    strings.join("\n")
}

fn gen_rodata(table: &HashMap<Number, String>, a: f64, b: f64) -> String {
    let elems = table.keys().zip(table.values());
    let mut strings: Vec<String> = vec!["section .rodata".to_string()
    , format!("    a dq {:?}", a)
    , format!("    b dq {:?}", b)];
    strings.append(&mut elems.map(|(key, value)| {
        format!("    {} dq {:?}", value, key.to_number())
    }).collect());
    strings.join("\n")
}

fn gen_text(table: &HashMap<Number, String>, functions: &[AST], derivatives: &[AST]) -> String {
    let fcount = functions.len();
    let dcount = derivatives.len();
    let functions = functions.iter().zip(1..(fcount + 1));
    let derivatives = derivatives.iter().zip(1..(dcount + 1));
    let mut lines = vec!["section .text".to_string()];
    lines.append(&mut functions.flat_map(|func| subroutine(table, "f", func)).collect());
    lines.append(&mut derivatives.flat_map(|func| subroutine(table, "df", func)).collect());
    lines.join("\n")
}

fn subroutine(table: &HashMap<Number, String>, prefix: &str, func: (&AST, usize)) -> Vec<String> {
    let (function, number) = func;
    let mut prolog = vec![ "push rbp"
                           , "mov rbp, rsp"
                           , "sub rsp, 8"
                           , "movsd qword[rsp], xmm0" ]
        .into_iter()
        .map(|str| format!("    {}", str))
        .collect();
    let mut epilog = vec![ "movsd xmm0, qword[rsp]"
                            , "add rsp, 16"
                            , "pop rbp"
                            , "ret" ]
        .into_iter()
        .map(|str| format!("    {}", str))
        .collect();
    let mut lines = vec![format!("{}{}:", prefix, number)];
    lines.append(&mut prolog);
    lines.append(&mut function.collect_info(&|node| gen_node(table, node)));
    lines.append(&mut epilog);
    lines
}

fn gen_node(table: &HashMap<Number, String>, node: &Link<Node>) -> Vec<String> {
    match *(*node.borrow()) {

        Node::Variable => vec!["    push qword[rbp - 8]".to_string()],

        Node::Number(x) => vec![format!("    mov rax, qword[{}]", table[&Number::new(x)])
        , "    push rax".to_string()],

        Node::UnaryOperator(ref token, _) => {
            let mut lines = vec!["fld qword[rsp]"];
            let mut body = match token.as_str() {
                "sin" => vec![ "fsin" ],
                "cos" => vec![ "fcos" ],
                "tan" => vec![ "fptan"
                             , "fstp st0" ],
                "ctg" => vec![ "fptan"
                             , "fdivp" ],
                "ln"  => vec![ "fld1"
                             , "fxch"
                             , "fy2lx" ],
                _ => Vec::new()
            };
            lines.append(&mut body);
            lines.push("fstp qword[rsp]");
            lines.into_iter().map(|str| format!("    {}", str)).collect()
        }

        Node::BinaryOperator(ref token, _, _) => {
            let mut lines = vec![ "fld qword[rsp + 8]"
                                           , "fld qword[rsp]" ];
            let mut body = match token.as_str() {
                "+" => vec![ "faddp" ],
                "-" => vec![ "fsubp" ],
                "*" => vec![ "fmulp" ],
                "/" => vec![ "fdivp" ],
                "^" => vec![ "fxch"
                           , "fyl2x"
                           , "fld1"
                           , "fld st1"
                           , "fprem"
                           , "f2xm1"
                           , "faddp"
                           , "fscale"
                           , "fstp st1" ],
                _ => Vec::new()
            };
            lines.append(&mut body);
            lines.push("add rsp, 8");
            lines.push("fstp qword[rsp]");
            lines.into_iter().map(|str| format!("    {}", str)).collect()
        }
    }
}