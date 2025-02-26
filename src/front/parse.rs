//! The parser

use std::fmt::Debug;

use derive_more::derive::Display;

use super::ast::*;
use super::lex::*;
use crate::common::id;

#[derive(Display)]
#[display("Parse error: {}", self.0)]
pub struct ParseError(String);

impl Debug for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

type ParseResult<T> = Result<T, ParseError>;

pub fn parse(input: &str) -> Result<Program, ParseError> {
    let mut parser = Parser::new(input);
    let program = parser.parse_program()?;
    if !parser.tokens.is_empty() {
        Err(ParseError(
            "There are still leftover tokens after reading a whole program.".to_string(),
        ))
    } else {
        Ok(program)
    }
}

struct Parser<'input> {
    /// Rest of the input, ordered in reverse.
    tokens: Vec<Token<'input>>,
}

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        let mut tokens = get_tokens(input);
        tokens.reverse();
        Parser { tokens }
    }

    // Fetch the next token without consuming
    //
    // Fails at the end of the input
    fn peek(&self) -> Option<Token> {
        self.tokens.last().copied()
    }

    // Get token and consume immediately
    fn next(&mut self) -> ParseResult<Token> {
        self.tokens
            .pop()
            .ok_or(ParseError("Unexpected end of input.".to_owned()))
    }

    // Extension of peek: Check if next token is what I want without consuming it
    fn next_is(&self, kind: TokenKind) -> bool {
        self.peek().map(|t| t.kind == kind).unwrap_or(false)
    }

    // Consume the token of given kind if able, return whether it is consumed

    // (e, e, e) -> while eat(comma) { read expr; build AST; } expect(RParen)?; (throw away upon failure)
    // If it fails, can handle the failure
    fn eat(&mut self, kind: TokenKind) -> bool {
        if self.next_is(kind) {
            self.tokens.pop();
            true
        } else {
            false
        }
    }

    // Expect a token of given kind
    fn expect(&mut self, kind: TokenKind) -> ParseResult<Token> {
        if self.next_is(kind) {
            self.next()
        } else if let Some(actual) = self.peek() {
            Err(ParseError(format!(
                "Expected a token with kind {kind}, found a token with kind {} and text `{}`.",
                actual.kind, actual.text
            )))
        } else {
            Err(ParseError(format!(
                "Expected a token with kind {kind} but reached the end of input."
            )))
        }
    }

    fn parse_program(&mut self) -> ParseResult<Program> {
        let mut stmts = vec![];

        // while !self.tokens.is_empty() {
        //     // Recogniser - Recognises when it fails but does not build an AST
        //     // For error handling: breaks if Err
        //     self.parse_stmt()?;
        // }
        while !self.tokens.is_empty() {
            stmts.push(self.parse_stmt()?);
        }

        Ok(Program { stmts })
    }

    fn parse_stmt(&mut self) -> ParseResult<Stmt> {
        // Where should my cursor be?
        // println!({:?}, parser.peek());
        let tok = self.next()?;

        // No epsilon case for this grammar
        match tok.kind {
            TokenKind::Assign => {
                let id = id(self.expect(TokenKind::Id)?.text);
                let exp = self.parse_expr()?;

                Ok(Stmt::Assign(id, exp))
            }
            TokenKind::Print => Ok(Stmt::Print(self.parse_expr()?)),
            TokenKind::Read => Ok(Stmt::Read(id(self.expect(TokenKind::Id)?.text))),
            TokenKind::If => Ok(Stmt::If {
                guard: self.parse_expr()?,
                tt: self.parse_block()?,
                ff: self.parse_block()?,
            }),
            _ => Err(ParseError(format!(
                "Expected start of a statement, found {}",
                tok.text
            ))),
        }
    }

    fn parse_block(&mut self) -> ParseResult<Vec<Stmt>> {
        let mut stmts = vec![];

        self.expect(TokenKind::LBrace)?; // Immediately fails if it does not start with {
        while !self.eat(TokenKind::RBrace) {
            // Read until I hit a }; Eat consumes if able (Will only consume RBrace)
            stmts.push(self.parse_stmt()?);
        }

        // Equivalent to above, except we split eat command into next_is() and expect()
        // self.expect(TokenKind::LBrace)?;            // Immediately fails if it does not start with {
        // while !self.next_is(TokenKind::RBrace) {    // Read until I hit a }
        //     stmts.push(self.parse_stmt()?);
        // }
        // self.expect(TokenKind::RBrace);

        Ok(stmts)
    }

    fn parse_expr(&mut self) -> ParseResult<Expr> {
        use Expr::*;

        let tok = self.next()?;

        match tok.kind {
            // Eg. kind: Id, text: "x"; We don't want to constantly compare text though because it is very slow, so we compare ptrs (numbers)
            TokenKind::Id => Ok(Var(id(tok.text))),
            // We know this unwrap cannot fail, otherwise the parser is broken
            TokenKind::Num => Ok(Const(tok.text.parse::<i64>().unwrap())),
            TokenKind::Plus => Ok(BinOp {
                op: BOp::Add,
                lhs: Box::new(self.parse_expr()?),
                rhs: Box::new(self.parse_expr()?),
            }),
            TokenKind::Minus => Ok(BinOp {
                op: BOp::Sub,
                lhs: Box::new(self.parse_expr()?),
                rhs: Box::new(self.parse_expr()?),
            }),
            TokenKind::Mul => Ok(BinOp {
                op: BOp::Mul,
                lhs: Box::new(self.parse_expr()?),
                rhs: Box::new(self.parse_expr()?),
            }),
            TokenKind::Div => Ok(BinOp {
                op: BOp::Div,
                lhs: Box::new(self.parse_expr()?),
                rhs: Box::new(self.parse_expr()?),
            }),
            TokenKind::Lt => Ok(BinOp {
                op: BOp::Lt,
                lhs: Box::new(self.parse_expr()?),
                rhs: Box::new(self.parse_expr()?),
            }),
            TokenKind::Tilde => Ok(Negate(Box::new(self.parse_expr()?))),
            _ => Err(ParseError(format!(
                "Expected start of a statement, found {}",
                tok.text
            ))),
        }
    }

    // helper: read and parse both sides of given binary operation
    fn parse_binop(&mut self, op: BOp) -> ParseResult<Expr> {
        let lhs = Box::new(self.parse_expr()?);
        let rhs = Box::new(self.parse_expr()?);
        Ok(Expr::BinOp { op, lhs, rhs })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use BOp::*;
    use Expr::*;
    use Stmt::*;

    // SECTION: helpers

    // Move a value to the heap
    fn b<T>(x: T) -> Box<T> {
        Box::new(x)
    }

    // Build a binary operation expression
    fn bop(op: BOp, lhs: Expr, rhs: Expr) -> Expr {
        BinOp {
            op,
            lhs: b(lhs),
            rhs: b(rhs),
        }
    }

    // Build a negation expression
    fn negate(inner: Expr) -> Expr {
        Negate(b(inner))
    }

    // Build a variable node
    fn var(name: &str) -> Expr {
        Var(id(name))
    }

    // SECTION: tests

    #[test]
    fn empty() {
        assert_eq!(parse("").unwrap().stmts, vec![]);
    }

    #[test]
    fn print() {
        assert_eq!(parse("$print 0").unwrap().stmts, vec![Print(Const(0))]);
    }

    #[test]
    fn read() {
        assert_eq!(parse("$read x").unwrap().stmts, vec![Read(id("x"))]);
    }

    #[test]
    fn var_test() {
        assert_eq!(parse("$print x").unwrap().stmts, vec![Print(var("x"))]);
    }

    #[test]
    fn binop() {
        assert_eq!(
            parse("$print + x x").unwrap().stmts,
            vec![Print(bop(Add, var("x"), var("x")))]
        );
        assert_eq!(
            parse("$print * x x").unwrap().stmts,
            vec![Print(bop(Mul, var("x"), var("x")))]
        );
        assert_eq!(
            parse("$print / x x").unwrap().stmts,
            vec![Print(bop(Div, var("x"), var("x")))]
        );
        assert_eq!(
            parse("$print - x x").unwrap().stmts,
            vec![Print(bop(Sub, var("x"), var("x")))]
        );
        assert_eq!(
            parse("$print < x x").unwrap().stmts,
            vec![Print(bop(Lt, var("x"), var("x")))]
        );
    }

    #[test]
    fn negate_test() {
        assert_eq!(
            parse("$print ~ x").unwrap().stmts,
            vec![Print(negate(var("x")))]
        );
    }

    #[test]
    fn complex_expr() {
        assert_eq!(
            parse("$print * + x 3 / ~ 7 y").unwrap().stmts,
            vec![Print(bop(
                Mul,
                bop(Add, var("x"), Const(3)),
                bop(Div, negate(Const(7)), var("y"))
            ))]
        );
    }

    #[test]
    fn assign() {
        assert_eq!(
            parse(":= x 3").unwrap().stmts,
            vec![Assign(id("x"), Const(3))]
        );
        assert_eq!(
            parse(":= x + x 3").unwrap().stmts,
            vec![Assign(id("x"), bop(Add, var("x"), Const(3)))]
        );
    }

    #[test]
    fn if_test() {
        assert_eq!(
            parse("$if x {} {}").unwrap().stmts,
            vec![If {
                guard: var("x"),
                tt: vec![],
                ff: vec![]
            }]
        );
        assert_eq!(
            parse("$if x {$print 0} {:= x 3}").unwrap().stmts,
            vec![If {
                guard: var("x"),
                tt: vec![Print(Const(0))],
                ff: vec![Assign(id("x"), Const(3))]
            }]
        );
        assert_eq!(
            parse("$if x {$print 0 $read x} {:= x 3 := y x}")
                .unwrap()
                .stmts,
            vec![If {
                guard: var("x"),
                tt: vec![Print(Const(0)), Read(id("x"))],
                ff: vec![Assign(id("x"), Const(3)), Assign(id("y"), var("x"))]
            }]
        );
        assert_eq!(
            parse("$if < x y {$print 0} {:= x 3}").unwrap().stmts,
            vec![If {
                guard: bop(Lt, var("x"), var("y")),
                tt: vec![Print(Const(0))],
                ff: vec![Assign(id("x"), Const(3))]
            }]
        );
    }

    #[test]
    fn death_test1() {
        // illegal tokens to start a program
        assert!(parse("x").is_err());
        assert!(parse("0").is_err());
        assert!(parse("<").is_err());

        // extra lexemes after a statement
        assert!(parse(":= x y + z").is_err());
        assert!(parse(":= x y + z t").is_err());
    }

    #[test]
    fn death_test_print() {
        // After $print, we call parse_expr which fails because there is no expr to parse
        // Only the main computer binary should crash/panic; the rest should return an error
        assert!(parse("$print").is_err());
    }

    #[test]
    fn death_test_read() {
        assert!(parse("$read").is_err());
    }

    #[test]
    fn death_test_assign() {
        assert!(parse(":=").is_err());
        assert!(parse(":= x").is_err());
        assert!(parse(":= 3 x").is_err());
    }

    #[test]
    fn death_test_if() {
        assert!(parse("$if").is_err());
        assert!(parse("$if x {}").is_err());
        assert!(parse("$if {} {}").is_err());
        assert!(parse("$if x y {}").is_err());
        assert!(parse("$if x $print x {}").is_err());
    }

    #[test]
    fn death_test_expr() {
        assert!(parse("$print 3 + x").is_err());
        assert!(parse("$print + x").is_err());
        assert!(parse("$print - x").is_err());
        assert!(parse("$print * x").is_err());
        assert!(parse("$print / x").is_err());
        assert!(parse("$print < x").is_err());
        assert!(parse("$print ~").is_err());
        assert!(parse("$print ~ x y").is_err());
        assert!(parse("$print + + x y").is_err());
        assert!(parse("$print < y").is_err());
        assert!(parse("$print < - y z").is_err());
    }
}
