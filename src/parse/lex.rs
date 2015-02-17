use std::sync::mpsc;

#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
pub struct Item {
    typ: ItemType,
    pos: usize,
    val: &'static str
}

pub struct Lexer {
    input: &'static str,
    left_delim: &'static str,
    right_delim: &'static str,
    state: StateFn,
    pos: usize,
    start: usize,
    width: usize,
    last_pos: usize,
    items_tx: mpsc::Sender<Item>,
    items_rx: mpsc::Receiver<Item>,
    paren_depth: usize
}

#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
pub enum ItemType {
    Error,
    Bool,
    Char,
    CharConstant,
    Complex,
    ColonEquals,
    EOF,
    Field,
    Identifier,
    LeftDelim,
    LeftParen,
    Number,
    Pipe,
    RawString,
    RightDelim,
    RightParen,
    Space,
    String,
    Text,
    Variable,
    Keyword,
    Dot,
    Define,
    Else,
    End,
    If,
    Nil,
    Range,
    Template,
    With
}

enum NextChar {
    Char(char),
    EOF
}

struct StateFn(fn(&mut Lexer) -> Option<StateFn>);

const LEFT_COMMENT: &'static str = "/*";
const RIGHT_COMMENT: &'static str = "*/";

impl Lexer {
    pub fn new(input: &'static str, left: &'static str, right: &'static str) -> Lexer {
        let (tx, rx) = mpsc::channel();

        Lexer {
            input: input,
            left_delim: match left { "" => "{{", anything => anything },
            right_delim: match right { "" => "}}", anything => anything },
            state: StateFn(lex_text),
            pos: 0,
            start: 0,
            width: 0,
            last_pos: 0,
            items_tx: tx,
            items_rx: rx,
            paren_depth: 0
        }
    }

    pub fn emit(&mut self, item_type: ItemType) {
        self.items_tx.send(
            Item {
                typ: item_type,
                pos: self.start,
                val: &self.input[self.start..self.pos]
            }
        );
        self.start = self.pos;
    }

    pub fn ignore(&mut self) {
        self.start = self.pos;
    }

    pub fn next(&mut self) -> NextChar {
        if self.pos >= self.input.len() {
            self.width = 0;
            return NextChar::EOF;
        }

        let next_char = self.input.char_at(self.pos);
        self.width = next_char.len_utf8();
        self.pos += self.width;
        return NextChar::Char(next_char);
    }

    pub fn next_item(&mut self) -> Item {
        let item = self.items_rx.recv().unwrap();
        self.last_pos = item.pos;
        item
    }

    pub fn run(&mut self) {
        loop {
            let StateFn(func) = self.state;

            match func(self) {
                Some(new_statefn) => { self.state = new_statefn; },
                None => break
            }
        }
    }
}

fn lex_comment(lexer: &mut Lexer) -> Option<StateFn> {
    lexer.pos += LEFT_COMMENT.len();

    match lexer.input[lexer.pos..].find_str(RIGHT_COMMENT) {
        Some(i) => {
            lexer.pos += i + RIGHT_COMMENT.len();

            if !lexer.input[lexer.pos..].starts_with(lexer.right_delim) {
                panic!("comment must be followed by closing delimiter");
            }

            lexer.pos += lexer.right_delim.len();
            lexer.ignore();
            return Some(StateFn(lex_text));
        },
        None => panic!("unclosed comment")
    }
}

fn lex_inside_action(lexer: &mut Lexer) -> Option<StateFn> {
    None
}

fn lex_left_delim(lexer: &mut Lexer) -> Option<StateFn> {
    lexer.pos += lexer.left_delim.len();

    if lexer.input[lexer.pos..].starts_with(LEFT_COMMENT) {
        return Some(StateFn(lex_comment));
    }

    lexer.emit(ItemType::LeftDelim);
    lexer.paren_depth = 0;
    return Some(StateFn(lex_inside_action));
}

fn lex_text(lexer: &mut Lexer) -> Option<StateFn> {
    loop {
        if lexer.input[lexer.pos..].starts_with(lexer.left_delim) {
            if lexer.pos > lexer.start {
                lexer.emit(ItemType::Text);
            }

            return Some(StateFn(lex_left_delim));
        }

        match lexer.next() {
            NextChar::EOF => break,
            _ => {},
        }
    }

    if lexer.pos > lexer.start {
        lexer.emit(ItemType::Text);
    }

    lexer.emit(ItemType::EOF);

    None
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lex(input: &'static str, left: &'static str, right: &'static str) -> Vec<Item> {
        let mut items: Vec<Item> = vec![];
        let mut lexer = Lexer::new(input, left, right);
        lexer.run();

        loop {
            let item = lexer.next_item();
            items.push(item.clone());

            match item.typ {
                ItemType::EOF | ItemType::Error => break,
                _ => { }
            }
        }

        items
    }

    macro_rules! test_cases {
        ($([$name:ident, $input:expr, $items:expr]),+) => (
            $(
                #[test]
                fn $name() {
                    let input = $input;
                    let expected_items = $items;
                    let actual_items = lex(input, "", "");
                    assert_eq!(expected_items, actual_items);
                }
            )+
        );
    }

    test_cases!(
        [
            empty,
            "",
            vec![
                Item { typ: ItemType::EOF, pos: 0, val: "" }
            ]
        ],
        [
            spaces,
            " \t\n",
            vec![
                Item { typ: ItemType::Text, pos: 0, val: " \t\n" },
                Item { typ: ItemType::EOF, pos: 3, val: "" }
            ]
        ],
        [
            text,
            "now is the time",
            vec![
                Item { typ: ItemType::Text, pos: 0, val: "now is the time" },
                Item { typ: ItemType::EOF, pos: 15, val: "" }
            ]
        ],
        [
            text_with_comment,
            "hello-{{/* this is a comment */}}-world",
            vec![
                Item { typ: ItemType::Text, pos: 0, val: "hello-" },
                Item { typ: ItemType::Text, pos: 33, val: "-world" },
                Item { typ: ItemType::EOF, pos: 39, val: "" }
            ]
        ]
    );
}
