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
    paren_depth: isize
}

#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialEq)]
enum ItemTypeKeyword {
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
    Keyword(ItemTypeKeyword),
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

struct StateFn(fn(&mut Lexer) -> Option<StateFn>);

type NextChar = Option<char>;

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

    pub fn accept(&mut self, valid_chars: &'static str) -> bool {
        if valid_chars.contains_char(self.next().unwrap()) {
            return true
        }

        self.backup();

        false
    }

    pub fn accept_run(&mut self, valid_chars: &'static str) {
        loop {
            if !valid_chars.contains_char(self.next().unwrap()) {
                break;
            }
        }

        self.backup();
    }

    pub fn at_terminator(&mut self) -> bool {
        true
    }

    pub fn backup(&mut self) {
        self.pos -= self.width;
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
            return None;
        }

        let next_char = self.input.char_at(self.pos);
        self.width = next_char.len_utf8();
        self.pos += self.width;

        Some(next_char)
    }

    pub fn next_item(&mut self) -> Item {
        let item = self.items_rx.recv().unwrap();
        self.last_pos = item.pos;
        item
    }

    pub fn peek(&mut self) -> NextChar {
        let next_char = self.next();
        self.backup();
        next_char
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

    pub fn scan_number(&mut self) -> bool {
        self.accept("+-");

        let digits = if self.accept("0") && self.accept("xX") {
            "0123456789abcdefABCDEF"
        } else {
            "0123456789"
        };

        self.accept_run(digits);

        if self.accept(".") {
            self.accept_run(digits);
        }

        if self.accept("eE") {
            self.accept("+-");
            self.accept_run("0123456789");
        }

        self.accept("i");

        if self.peek().unwrap().is_alphanumeric() {
            self.next();

            return false;
        }

        true
    }
}

fn is_space(c: char) -> bool {
    c == ' ' || c == '\t'
}

fn item_type_keyword_from_string(string: &str) -> Option<ItemTypeKeyword> {
    match string {
        "." => Some(ItemTypeKeyword::Dot),
        "define" => Some(ItemTypeKeyword::Define),
        "else" => Some(ItemTypeKeyword::Else),
        "end" => Some(ItemTypeKeyword::End),
        "if" => Some(ItemTypeKeyword::If),
        "range" => Some(ItemTypeKeyword::Range),
        "nil" => Some(ItemTypeKeyword::Nil),
        "template" => Some(ItemTypeKeyword::Template),
        "with" => Some(ItemTypeKeyword::With),
        _ => None
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

fn lex_identifier(lexer: &mut Lexer) -> Option<StateFn> {
    loop {
        let c = lexer.next().unwrap();

        if !c.is_alphanumeric() {
            break;
        }
    }

    lexer.backup();

    let word = &lexer.input[lexer.start..lexer.pos];

    if !lexer.at_terminator() {
        panic!("bad character");
    }

    match item_type_keyword_from_string(word) {
        Some(keyword) => { lexer.emit(ItemType::Keyword(keyword)) },
        None => {
            if word.char_at(0) == '.' {
                lexer.emit(ItemType::Field);
            } else if word == "true" || word == "false" {
                lexer.emit(ItemType::Bool);
            } else {
                lexer.emit(ItemType::Identifier);
            }
        }
    }

    Some(StateFn(lex_inside_action))
}

fn lex_inside_action(lexer: &mut Lexer) -> Option<StateFn> {
    if lexer.input[lexer.pos..].starts_with(lexer.right_delim) {
        if lexer.paren_depth == 0 {
            return Some(StateFn(lex_right_delim));
        }

        panic!("unclosed left paren");
    }

    match lexer.next() {
        Some(c) => {
            if is_space(c) {
                return Some(StateFn(lex_space));
            } else if c == '"' {
                return Some(StateFn(lex_quote));
            } else if c == '+' || c == '-' || ('0' <= c && c <= '9') {
                lexer.backup();

                return Some(StateFn(lex_number));
            } else if c.is_alphanumeric() {
                lexer.backup();
                return Some(StateFn(lex_identifier));
            } else if c == '(' {
                lexer.emit(ItemType::LeftParen);
                lexer.paren_depth += 1;

                return Some(StateFn(lex_inside_action));
            } else if c == ')' {
                lexer.emit(ItemType::RightParen);
                lexer.paren_depth -= 1;

                if lexer.paren_depth < 0 {
                    panic!("unexpected right paren");
                }

                return Some(StateFn(lex_inside_action));
            } else {
                lexer.emit(ItemType::Char);

                return Some(StateFn(lex_inside_action));
            }
        },
        None => panic!("unclosed action")
    }

    Some(StateFn(lex_inside_action))
}

fn lex_left_delim(lexer: &mut Lexer) -> Option<StateFn> {
    lexer.pos += lexer.left_delim.len();

    if lexer.input[lexer.pos..].starts_with(LEFT_COMMENT) {
        return Some(StateFn(lex_comment));
    }

    lexer.emit(ItemType::LeftDelim);
    lexer.paren_depth = 0;
    Some(StateFn(lex_inside_action))
}

fn lex_number(lexer: &mut Lexer) -> Option<StateFn> {
    if !lexer.scan_number() {
        panic!("bad number syntax");
    }

    lexer.emit(ItemType::Number);

    Some(StateFn(lex_inside_action))
}

fn lex_quote(lexer: &mut Lexer) -> Option<StateFn> {
    loop {
        match lexer.next().expect("unterminated quoted string") {
            '\\' => {
                if lexer.next().expect("unterminated quoted string") == '\n' {
                    panic!("unterminated quoted string");
                }
            },
            '\n' => panic!("unterminated quoted string"),
            '"' => break,
            _ => {}
        }
    }

    lexer.emit(ItemType::String);
    Some(StateFn(lex_inside_action))
}

fn lex_right_delim(lexer: &mut Lexer) -> Option<StateFn> {
    lexer.pos += lexer.right_delim.len();
    lexer.emit(ItemType::RightDelim);
    Some(StateFn(lex_text))
}

fn lex_space(lexer: &mut Lexer) -> Option<StateFn> {
    loop {
        if !is_space(lexer.peek().unwrap()) {
            break;
        }
    }

    lexer.emit(ItemType::Space);

    Some(StateFn(lex_inside_action))
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
            Some(_) => {},
            None => break,
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
        ($([$name:ident, $input:expr, [$([$ty:ident, $pos:expr, $val:expr]),+]]),+) => (
            $(
                #[test]
                fn $name() {
                    let input = $input;
                    let expected_items = vec![$(Item { typ: ItemType::$ty, pos: $pos, val: $val }),+];
                    let actual_items = lex(input, "", "");
                    assert_eq!(expected_items, actual_items);
                }
            )+
        );
    }

    test_cases!(
        [empty, "", [[EOF, 0, ""]]],
        [spaces, " \t\n", [[Text, 0, " \t\n"], [EOF, 3, ""]]],
        [text, "now is the time", [[Text, 0, "now is the time"], [EOF, 15, ""]]],
        [text_with_comment, "hello-{{/* this is a comment */}}-world", [
            [Text, 0, "hello-"],
            [Text, 33, "-world"],
            [EOF, 39, ""]
        ]],
        [punctuation, "{{,@% }}", [
            [LeftDelim, 0, "{{"],
            [Char, 2, ","],
            [Char, 3, "@"],
            [Char, 4, "%"],
            [Space, 5, " "],
            [RightDelim, 6, "}}"],
            [EOF, 8, ""]
        ]],
        [parens, "{{((3))}}", [
            [LeftDelim, 0, "{{"],
            [LeftParen, 2, "("],
            [LeftParen, 3, "("],
            [Number, 4, "3"],
            [RightParen, 5, ")"],
            [RightParen, 6, ")"],
            [RightDelim, 7, "}}"],
            [EOF, 9, ""]
        ]],
        [empty_action, "{{}}", [[LeftDelim, 0, "{{"], [RightDelim, 2, "}}"], [EOF, 4, ""]]],
        [for_, "{{for}}", [
            [LeftDelim, 0, "{{"],
            [Identifier, 2, "for"],
            [RightDelim, 5, "}}"],
            [EOF, 7, ""]
        ]],
        [quote, r#"{{"abc \n\t\" "}}"#, [
            [LeftDelim, 0, "{{"],
            [String, 2, r#""abc \n\t\" ""#],
            [RightDelim, 15, "}}"],
            [EOF, 17, ""]
        ]]
    );
}
