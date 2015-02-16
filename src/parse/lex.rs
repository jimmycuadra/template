#[derive(Clone)]
pub struct Item {
    typ: ItemType,
    pos: Position,
    val: &'static str
}

pub struct Lexer {
    unused: ()
}

#[derive(Clone)]
pub enum ItemType {
    ItemError,
    ItemBool,
    ItemChar,
    ItemCharConstant,
    ItemComplex,
    ItemColonEquals,
    ItemEOF
}

type Position = u8;

impl Lexer {
    pub fn new(input: &str, left: &str, right: &str) -> Lexer {
        Lexer {
            unused: ()
        }
    }

    pub fn next_item(&self) -> Item {
        Item {
            typ: ItemType::ItemEOF,
            pos: 0,
            val: ""
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const T_EOF: Item = Item{
            typ: ItemType::ItemEOF,
            pos: 0,
            val: ""
    };

    fn lex(input: &str, left: &str, right: &str) -> Vec<Item> {
        let mut items: Vec<Item> = vec![];
        let lexer = Lexer::new(input, left, right);

        loop {
            let item = lexer.next_item();
            items.push(item.clone());

            match item.typ {
                ItemType::ItemEOF | ItemType::ItemError => { break; },
                _ => { }
            }
        }

        items
    }

    fn equal(actual: Vec<Item>, expected: Vec<Item>) -> bool {
        false
    }

    #[test]
    fn empty() {
        let input = "";
        let expected_items = vec![T_EOF];
        let actual_items = lex(input, "", "");
        assert!(equal(actual_items, expected_items))
    }
}
