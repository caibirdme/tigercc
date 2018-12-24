#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Illegal,
    EOF,
    // reserved
    While,
    For,
    Break,
    Continue,
    Let,
    Function,
    Type,
    If,
    Else,
    Nil,
    Return,

    // punctuation
    Comma, // ,
    Colon, // :
    SemiColon, // ;
    LParen,  // (
    RParen, // )
    LBracket, // [
    RBracket, // ]
    LBrace, // {
    RBrace, // }
    Dot, // .

    // operator
    Add,
    Sub,
    Mul,
    Div,
    Assign,
    NotEqual,
    Equal,
    Less,
    LessEqual,
    Large,
    LargeEqual,
    BinAnd,
    BinOr,
    BinNot,
    BinXor,
    LogicAnd,
    LogicOr,

    Ident(String),
    StrLit(String),
    BoolLit(bool),
    IntLit(i64),
    FloatLit(f64),
}