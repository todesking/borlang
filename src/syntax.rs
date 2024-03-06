#[rust_sitter::grammar("borlang")]
pub mod grammar {
    #[rust_sitter::language]
    #[derive(Debug)]
    pub enum Program {
        Expr(Expr),
    }

    #[rust_sitter::extra]
    pub struct Whitespace(#[rust_sitter::leaf(pattern = r"\s+")] ());

    #[derive(Debug)]
    pub enum Expr {
        Int(#[rust_sitter::leaf(pattern = r"\d+", transform = |s| s.parse().unwrap())] i32),
        #[rust_sitter::prec_left(1)]
        BinOpAddSub {
            lhs: Box<Expr>,
            op: OpAddSub,
            rhs: Box<Expr>,
        },
        #[rust_sitter::prec_left(2)]
        BinOpMul {
            lhs: Box<Expr>,
            op: OpMul,
            rhs: Box<Expr>,
        },
        #[rust_sitter::prec(9)]
        App {
            expr: Box<Expr>,
            _lparen: LParen,
            #[rust_sitter::repeat]
            #[rust_sitter::delimited(
                #[rust_sitter::leaf(text = ",")]
                ()
            )]
            args: Vec<Expr>,
            _rparen: RParen,
        },
        Let {
            #[rust_sitter::leaf(text = "let")]
            _let: (),
            name: Ident,
            #[rust_sitter::leaf(text = "=")]
            _eq: (),
            expr: Box<Expr>,
        },
        Var(Ident),
        Block(BlockExpr),
        Paren {
            _lparen: LParen,
            expr: Box<Expr>,
            #[rust_sitter::leaf(text = ")")]
            _rparen: RParen,
        },
        Fn {
            #[rust_sitter::leaf(text = "fn")]
            _fun: (),
            ident: Option<Ident>,
            _lparen: LParen,
            #[rust_sitter::delimited(
                #[rust_sitter::leaf(text=",")]
            )]
            params: Vec<Ident>,
            _rparen: RParen,
            expr: BlockExpr,
        },
        If {
            #[rust_sitter::leaf(text = "if")]
            _if: (),
            _lparen: LParen,
            cond: Box<Expr>,
            _rparen: RParen,
            th: BlockExpr,
            #[rust_sitter::leaf(text = "else")]
            _else: (),
            el: BlockExpr,
        },
    }

    #[derive(Debug)]
    pub struct LParen(#[rust_sitter::leaf(text = "(")] ());
    #[derive(Debug)]
    pub struct RParen(#[rust_sitter::leaf(text = ")")] ());
    #[derive(Debug)]
    pub struct LBrace(#[rust_sitter::leaf(text = "{")] ());
    #[derive(Debug)]
    pub struct RBrace(#[rust_sitter::leaf(text = "}")] ());

    #[derive(Debug)]
    pub struct BlockExpr {
        _lbrace: LBrace,
        #[rust_sitter::delimited(
                #[rust_sitter::leaf(text=";")]
                ()
            )]
        pub exprs: Vec<Expr>,
        _rbrace: RBrace,
    }

    #[derive(Debug)]
    pub struct Ident(
        #[rust_sitter::leaf(pattern=r"[a-z][a-z0-9_]*", transform = |s| s.to_owned())] String,
    );

    #[derive(Debug)]
    pub enum OpAddSub {
        #[rust_sitter::leaf(text = "+")]
        Add,
        #[rust_sitter::leaf(text = "-")]
        Sub,
    }
    #[derive(Debug)]
    pub enum OpMul {
        #[rust_sitter::leaf(text = "*")]
        Mul,
    }
}

pub use grammar::parse;
pub use grammar::Expr;
pub use grammar::Program;

pub use rust_sitter::errors::ParseError;
