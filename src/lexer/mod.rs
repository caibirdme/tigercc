use nom::*;
use nom::types::*;

pub mod token;

use crate::lexer::token::*;
use std::str;
use std::str::FromStr;
use std::str::Utf8Error;

// operator
named!(
    add_op<CompleteByteSlice, Token>,
    do_parse!(tag!("+") >> (Token::Add))
);

named!(
    sub_op<CompleteByteSlice, Token>,
    do_parse!(tag!("-") >> (Token::Sub))
);

named!(
    mul_op<CompleteByteSlice, Token>,
    do_parse!(tag!("*") >> (Token::Mul))
);

named!(
    div_op<CompleteByteSlice, Token>,
    do_parse!(tag!("/") >> (Token::Div))
);

named!(
    assign_op<CompleteByteSlice, Token>,
    do_parse!(tag!("=") >> (Token::Assign))
);

named!(
    not_equal_op<CompleteByteSlice, Token>,
    do_parse!(tag!("!=") >> (Token::NotEqual))
);

named!(
    equal_op<CompleteByteSlice, Token>,
    do_parse!(tag!("==") >> (Token::Equal))
);

named!(
    less_op<CompleteByteSlice, Token>,
    do_parse!(tag!("<") >> (Token::Less))
);

named!(
    less_equal_op<CompleteByteSlice, Token>,
    do_parse!(tag!("<=") >> (Token::LessEqual))
);

named!(
    large_op<CompleteByteSlice, Token>,
    do_parse!(tag!(">") >> (Token::Large))
);

named!(
    large_equal_op<CompleteByteSlice, Token>,
    do_parse!(tag!(">=") >> (Token::LargeEqual))
);

named!(
    bin_and_op<CompleteByteSlice, Token>,
    do_parse!(tag!("&") >> (Token::BinAnd))
);

named!(
    bin_or_op<CompleteByteSlice, Token>,
    do_parse!(tag!("|") >> (Token::BinOr))
);

named!(
    bin_not_op<CompleteByteSlice, Token>,
    do_parse!(tag!("!") >> (Token::BinNot))
);

named!(
    bin_xor_op<CompleteByteSlice, Token>,
    do_parse!(tag!("^") >> (Token::BinXor))
);

named!(
    logic_or_op<CompleteByteSlice, Token>,
    do_parse!(tag!("||") >> (Token::LogicOr))
);

named!(
    logic_and_op<CompleteByteSlice, Token>,
    do_parse!(tag!("&&") >> (Token::LogicAnd))
);

named!(lex_operator<CompleteByteSlice, Token>, alt!(
    add_op |
    sub_op |
    mul_op |
    div_op |
    less_equal_op |
    less_op |
    large_equal_op |
    large_op |
    equal_op |
    not_equal_op |
    assign_op |
    logic_and_op |
    logic_or_op |
    bin_and_op |
    bin_or_op |
    bin_xor_op |
    bin_not_op
));

// punctuations
named!(comma_punctuation<CompleteByteSlice, Token>,
  do_parse!(tag!(",") >> (Token::Comma))
);

named!(semicolon_punctuation<CompleteByteSlice, Token>,
  do_parse!(tag!(";") >> (Token::SemiColon))
);

named!(colon_punctuation<CompleteByteSlice, Token>,
  do_parse!(tag!(":") >> (Token::Colon))
);

named!(lparen_punctuation<CompleteByteSlice, Token>,
  do_parse!(tag!("(") >> (Token::LParen))
);

named!(rparen_punctuation<CompleteByteSlice, Token>,
  do_parse!(tag!(")") >> (Token::RParen))
);

named!(lbrace_punctuation<CompleteByteSlice, Token>,
  do_parse!(tag!("{") >> (Token::LBrace))
);

named!(rbrace_punctuation<CompleteByteSlice, Token>,
  do_parse!(tag!("}") >> (Token::RBrace))
);

named!(lbracket_punctuation<CompleteByteSlice, Token>,
  do_parse!(tag!("[") >> (Token::LBracket))
);

named!(rbracket_punctuation<CompleteByteSlice, Token>,
  do_parse!(tag!("]") >> (Token::RBracket))
);

named!(lex_punctuations<CompleteByteSlice, Token>, alt!(
    comma_punctuation |
    semicolon_punctuation |
    colon_punctuation |
    lparen_punctuation |
    rparen_punctuation |
    lbrace_punctuation |
    rbrace_punctuation |
    lbracket_punctuation |
    rbracket_punctuation
));

// string

// Strings
fn pis(input: CompleteByteSlice) -> IResult<CompleteByteSlice, Vec<u8>> {
    use std::result::Result::*;

    let (i1, c1) = try_parse!(input, take!(1));
    match c1.as_bytes() {
        b"\"" => Ok((input, vec![])),
        b"\\" => {
            let (i2, c2) = try_parse!(i1, take!(1));
            pis(i2).map(|(slice, done)| (slice, concat_slice_vec(c2.0, done)))
        }
        c => {
            pis(i1).map(|(slice, done)| (slice, concat_slice_vec(c, done)))
        },
    }
}

fn concat_slice_vec(c: &[u8], done: Vec<u8>) -> Vec<u8> {
    let mut new_vec = c.to_vec();
    new_vec.extend(&done);
    new_vec
}

fn convert_vec_utf8(v: Vec<u8>) -> Result<String, Utf8Error> {
    let slice = v.as_slice();
    str::from_utf8(slice).map(|s| s.to_owned())
}

named!(string_lit<CompleteByteSlice, String>,
  delimited!(
    tag!("\""),
    map_res!(pis, convert_vec_utf8),
    tag!("\"")
  )
);

named!(lex_string<CompleteByteSlice, Token>,
    do_parse!(
        s: string_lit >>
        (Token::StrLit(s))
    )
);

// Reserved or identifier
fn parse_reserved(c: CompleteStr, rest: Option<CompleteStr>) -> Token {
    let mut string = c.0.to_owned();
    string.push_str(rest.unwrap_or(CompleteStr("")).0);
    match string.as_ref() {
        "while" => Token::While,
        "for" => Token::For,
        "break" => Token::Break,
        "continue" => Token::Continue,
        "let" => Token::Let,
        "fn" => Token::Function,
        "type" => Token::Type,
        "if" => Token::If,
        "else" => Token::Else,
        "return" => Token::Return,
        "nil" => Token::Nil,
        "true" => Token::BoolLit(true),
        "false" => Token::BoolLit(false),
        _ => Token::Ident(string),
    }
}

fn complete_byte_slice_str_from_utf8(c: CompleteByteSlice) -> Result<CompleteStr, Utf8Error> {
    str::from_utf8(c.0).map(|s| CompleteStr(s))
}

macro_rules! check(
  ($input:expr, $submac:ident!( $($args:tt)* )) => (
    {
      use std::result::Result::*;
      use nom::{Err,ErrorKind};

      let mut failed = false;
      for &idx in $input.0 {
        if !$submac!(idx, $($args)*) {
            failed = true;
            break;
        }
      }
      if failed {
        let e: ErrorKind<u32> = ErrorKind::Tag;
        Err(Err::Error(error_position!($input, e)))
      } else {
        Ok((&b""[..], $input))
      }
    }
  );
  ($input:expr, $f:expr) => (
    check!($input, call!($f));
  );
);

named!(take_1_char<CompleteByteSlice, CompleteByteSlice>,
    flat_map!(take!(1), check!(is_alphabetic))
);

named!(lex_reserved_ident<CompleteByteSlice, Token>,
    do_parse!(
        c: map_res!(call!(take_1_char), complete_byte_slice_str_from_utf8) >>
        rest: opt!(complete!(map_res!(alphanumeric, complete_byte_slice_str_from_utf8))) >>
        (parse_reserved(c, rest))
    )
);

fn complete_str_from_str<F: FromStr>(c: CompleteStr) -> Result<F, F::Err> {
    FromStr::from_str(c.0)
}

// Integers parsing
named!(lex_integer<CompleteByteSlice, Token>,
    do_parse!(
        i: map_res!(map_res!(digit, complete_byte_slice_str_from_utf8), complete_str_from_str) >>
        (Token::IntLit(i))
    )
);

// Illegal tokens
named!(lex_illegal<CompleteByteSlice, Token>,
    do_parse!(take!(1) >> (Token::Illegal))
);

named!(lex_token<CompleteByteSlice, Token>, alt_complete!(
    lex_operator |
    lex_punctuations |
    lex_string |
    lex_reserved_ident |
    lex_integer |
    lex_illegal
));

named!(lex_tokens<CompleteByteSlice, Vec<Token>>, ws!(many0!(lex_token)));


pub struct Lexer;

impl Lexer {
    pub fn lex_tokens(bytes: &[u8]) -> IResult<CompleteByteSlice, Vec<Token>> {
        lex_tokens(CompleteByteSlice(bytes)).map(|(slice, result)|
            (slice, [&result[..], &vec![Token::EOF][..]].concat())
        )
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer1() {
        let input = &b"=+(){},;"[..];
        let (_, result) = Lexer::lex_tokens(input).unwrap();

        let expected_results = vec![
            Token::Assign,
            Token::Add,
            Token::LParen,
            Token::RParen,
            Token::LBrace,
            Token::RBrace,
            Token::Comma,
            Token::SemiColon,
            Token::EOF,
        ];

        assert_eq!(result, expected_results);
    }

    #[test]
    fn test_lexer2() {
        let input = &b"  a+   b *c + d-   100    /  2  "[..];
        let (_, result) = Lexer::lex_tokens(input).unwrap();

        let expected_results = vec![
            Token::Ident("a".to_string()),
            Token::Add,
            Token::Ident("b".to_string()),
            Token::Mul,
            Token::Ident("c".to_string()),
            Token::Add,
            Token::Ident("d".to_string()),
            Token::Sub,
            Token::IntLit(100),
            Token::Div,
            Token::IntLit(2),
            Token::EOF,
        ];

        assert_eq!(result, expected_results);
    }

    #[test]
    fn test_lexer3() {
        let input = &b"== = === >>= <== >=="[..];
        let (_, result) = Lexer::lex_tokens(input).unwrap();

        let expected_results = vec![
            Token::Equal,
            Token::Assign,
            Token::Equal,
            Token::Assign,
            Token::Large,
            Token::LargeEqual,
            Token::LessEqual,
            Token::Assign,
            Token::LargeEqual,
            Token::Assign,
            Token::EOF,
        ];

        assert_eq!(result, expected_results);
    }

    #[test]
    fn test_lexer4() {
        let input = &b"\"I love rust\" + \"\\\"\"so do i return"[..];
        let (_, result) = Lexer::lex_tokens(input).unwrap();

        let expected_results = vec![
            Token::StrLit("I love rust".to_string()),
            Token::Add,
            Token::StrLit("\"".to_string()),
            Token::Ident("so".to_string()),
            Token::Ident("do".to_string()),
            Token::Ident("i".to_string()),
            Token::Return,
            Token::EOF,
        ];

        assert_eq!(result, expected_results);
    }
}