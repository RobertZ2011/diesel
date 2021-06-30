pub mod lexer;
pub mod token;
pub mod ast;
pub mod parse_error;

use std::{
    collections::VecDeque,
    iter::IntoIterator
};

use crate::parser::{
    ast::{
        Expr,
        BinOp,
        UnaryOp,
        Module,
        Definition,
        IfExpr
    },
    token::{
        Token,
        TokenValue,
        TokenStream,
        TokenType,
        Op,
        OpAssociativity
    },
    parse_error::{
        UnexpectedToken,
        OperandMismatch,
        ParseError
    }
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum OperatorValue<'a> {
    Op(Op),
    Function(&'a str),
    Paren,
    Block,
    If
}

#[derive(Clone, Debug)]
pub struct Operator<'a> {
    value: OperatorValue<'a>,
    token: Token<'a>
}

pub struct Parser<'a, T: IntoIterator<Item = Token<'a>>> {
    stream: TokenStream<'a, T>,
    module: Module,

    output: VecDeque<Box<Expr>>,
    func_args: VecDeque<usize>, //keeps track of arguments passed to a function
    block_args: VecDeque<usize> //keeps track of exprs passed to a block
}

impl<'a, T: IntoIterator<Item = Token<'a>>> Parser<'a, T> {
    pub fn new(stream: TokenStream<'a, T> ) -> Parser<'a, T> {
        Parser {
            stream: stream,
            module: Module::new(),

            output: VecDeque::new(),
            func_args: VecDeque::new(),
            block_args: VecDeque::new()
        }
    }

    pub fn parse(mut self) -> Result<Module, ParseError<'a>> {
        while let Some(token_type) = self.stream.peek_token_type() {
            match token_type {
                TokenType::Function => self.parse_function_def()?,
                _ => ()
            }
        }

        Ok(self.module)
    }

    fn parse_function_def(&mut self) -> Result<(), ParseError<'a>> {
        let _ = self.stream.expect_function().map_err(ParseError::UnexpectedToken)?;
        let name = self.stream.expect_identifier().map_err(ParseError::UnexpectedToken)?;
        let _ = self.stream.expect_lparen();

        let mut args: Vec<&str> = Vec::new();
        loop {
            let res = self.stream.consume();
            if res.is_none() {
                //Ran out of input
                return ParseError::unexpected_token(TokenType::Identifier, Token::eof());
            }
            else {
                let token_res = res.unwrap();
                let Token { value: token, .. } = token_res;
                if token == TokenValue::RParen {
                    //args list finished
                    break;
                }
                else if let TokenValue::Identifier(iden) = token {
                    args.push(iden);
                    if let Some(TokenType::Comma) = self.stream.peek_token_type() {
                        //Consume the comma if there is one
                        //If the next token isn't an identifier or comma, the next iteration will catch it
                        let _ = self.stream.consume();
                    }
                }
                else {
                    return ParseError::unexpected_token_multi(&[TokenType::Identifier, TokenType::RParen], token_res);
                }
            }
        }

        let expr = self.parse_expr()?;

        let def = Definition::Function(String::from(name), args.into_iter().map(String::from).collect(), expr);
        self.module.definitions.push(def);

        Ok(())
    }

    fn generate_operator_expr(&mut self, operator: Operator<'a>) -> Result<(), ParseError<'a>> {
        let Operator { value, token } = operator;
        if let OperatorValue::Op(op) = value {
            let len = self.output.len();
            if op.is_unary() {
                if len >= 1 {
                    let expr = self.output.pop_back().unwrap();
                    let unary_op = UnaryOp::from_op(op);
                    let new_expr = Box::new(Expr::UnaryOp(unary_op, expr));
                    self.output.push_back(new_expr);
                }
                else {
                    return ParseError::operand_mismatch(token, op, len);
                }
            }
            else {
                
                if len >= 2 {
                    let rhs = self.output.pop_back().unwrap();
                    let lhs = self.output.pop_back().unwrap();
                    let bin_op = BinOp::from_op(op);
                    let new_expr = Box::new(Expr::BinOp(bin_op, lhs, rhs));
                    self.output.push_back(new_expr);
                }
                else {
                    return ParseError::operand_mismatch(token, op, len);
                }
            }
        }
        else if let OperatorValue::Function(iden) = value {
            //Siphon all output to be arguments to the function
            let arg_count = self.func_args.pop_front().unwrap() + 1;

            let mut args = Vec::new();
            for _ in 0..arg_count {
                args.push(self.output.pop_back().unwrap());
            }

            let expr = Box::new(Expr::FunctionApp(String::from(iden), args));
            self.output.push_back(expr);
        }
        else if value == OperatorValue::Block {
             //Siphon off any remaining output into this block
            let block_count = self.block_args.pop_back().unwrap() + 1;

             let mut exprs = Vec::new();
             for _ in 0..block_count {
                exprs.push(self.output.pop_back().unwrap());
            }

            let expr = Box::new(Expr::Block(exprs));
            self.output.push_back(expr);
            
        }
        else if value == OperatorValue::If {
            if self.output.len() >= 3 {
                let else_expr = self.output.pop_back().unwrap();
                let then_expr = self.output.pop_back().unwrap();
                let cond_expr = self.output.pop_back().unwrap();

                let expr = Box::new(Expr::If(IfExpr {
                    cond: cond_expr,
                    then_expr: then_expr,
                    else_expr: else_expr
                }));

                self.output.push_back(expr);
            }
        }
        else {
            panic!("Unsupported operator {:?}", value);
        }

        Ok(())
    }

    fn parse_expr(&mut self) -> Result<Box<Expr>, ParseError<'a>> {
        let mut operators: VecDeque<Operator<'a>> = VecDeque::new();
        let mut if_level = 0;

        while let Some(token_type) = self.stream.peek_token_type() {
            println!("{:?}", self.output);
            println!("{:?}\n", operators);

            if token_type == TokenType::Int {
                let (_, value) = self.stream.consume_int();
                self.output.push_back(Box::new(Expr::ConstInt(value)));

                if if_level > 0 {
                    while let Some(operator) = operators.pop_front() {
                        let Operator { value: op, token } = operator;
                        if op != OperatorValue::If && op != OperatorValue::Block && op != OperatorValue::Paren {
                            let _ = self.generate_operator_expr(Operator { value: op, token: token })?;
                        }
                        else {
                            operators.push_front(Operator { value: op, token: token });
                            break;
                        }
                    }
                }
            }
            else if token_type == TokenType::Identifier {
                let (token, iden) = self.stream.consume_identifier();

                if let Some(next_type) = self.stream.peek_token_type() {
                    if next_type == TokenType::LParen {
                        //function application
                        let _ = self.stream.consume();
                        operators.push_front(Operator { value: OperatorValue::Function(iden), token: token });
                        self.func_args.push_front(0);
                    }
                    else {
                        //Just treat this as a variable for now
                        //TODO: make this check the next token
                        self.output.push_back(Box::new(Expr::Var(String::from(iden))));

                        if if_level > 0 {
                            while let Some(operator) = operators.pop_front() {
                                let op = operator.value;
                                if op != OperatorValue::If {
                                    let _ = self.generate_operator_expr(operator)?;
                                }
                                else {
                                    operators.push_front(operator);
                                    break;
                                }
                            }
                        }
                    }
                }
                else {
                    //No next token, just treat this an a variable
                    self.output.push_back(Box::new(Expr::Var(String::from(iden))));

                    if if_level > 0 {
                        while let Some(operator) = operators.pop_front() {
                            let op = operator.value;
                            if op != OperatorValue::If {
                                let _ = self.generate_operator_expr(operator)?;
                            }
                            else {
                                operators.push_front(operator);
                                break;
                            }
                        }
                    }
                }
            }
            else if token_type == TokenType::Operator {
                let (token, op) = self.stream.consume_op();

                while operators.len() > 0 {
                    let front = operators.front().unwrap();
                    let Operator { value, .. } = front;
                    if let OperatorValue::Op(top) = value {
                        let op_prec = op.precedence();
                        let op_assoc = op.associativity();
                        let top_prec = top.precedence();

                        if top_prec < op_prec || (op_prec == top_prec && op_assoc == OpAssociativity::Left) {
                            let op = operators.pop_front().unwrap();
                            let _ = self.generate_operator_expr(op)?;
                
                        }
                        else {
                            break;
                        }
                    }
                    else if *value == OperatorValue::Paren || *value == OperatorValue::Block || *value == OperatorValue::If {
                       break;
                    }
                    else if let OperatorValue::Function(_) = value {
                        break;
                    }
                    else {
                        panic!("Unsupported operator {:?}", operators.front().unwrap());
                    }
                }

                operators.push_front(Operator { value: OperatorValue::Op(op), token: token });
            }
            else if token_type == TokenType::LParen {
                let token = self.stream.consume().unwrap();
                operators.push_front(Operator { value: OperatorValue::Paren, token: token });
            }
            else if token_type == TokenType::RParen {
                let token = self.stream.consume().unwrap();
                if operators.len() == 0 {
                    //No operators means no matching parenthesis
                    //Return an error
                    return Err(ParseError::MismatchedParen(token));
                }
                
                let mut found_paren = false;
                while let Some(operator) = operators.pop_front() {
                    let op = operator.value;
                    if op == OperatorValue::Paren {
                        found_paren = true;
                        break;
                    }
                    else if let OperatorValue::Function(_) = op {
                        found_paren = true;

                        let _ =self.generate_operator_expr(operator)?;
                        break;
                    }
                    else {
                        let _ = self.generate_operator_expr(operator)?;
                    }
                }

                //Couldn't find a corresponding parenthesis, return an error
                if !found_paren {
                    return Err(ParseError::MismatchedParen(token));
                }
            }
            else if token_type == TokenType::LBrace {
                let token = self.stream.consume().unwrap();
                operators.push_front(Operator { value: OperatorValue::Block, token: token });
                self.block_args.push_back(0);
            }
            else if token_type == TokenType::RBrace {
                let token = self.stream.consume().unwrap();
                if operators.len() == 0 {
                    //No matching brace operator on the stack, return an error
                    return Err(ParseError::MismatchedBrace(token));
                }
                
                let mut found_brace = false;
                while let Some(operator) = operators.pop_front() {
                    let op = operator.value;
                    if op == OperatorValue::Block {
                        found_brace = true;

                        let _ = self.generate_operator_expr(operator)?;

                        break;
                    }
                    else {
                        let _ = self.generate_operator_expr(operator)?;
                    }
                }

                if !found_brace {
                    //No matching brace found, return an error
                    return Err(ParseError::MismatchedBrace(token));
                }
            }
            else if token_type == TokenType::If {
                let token = self.stream.consume().unwrap();
                operators.push_front(Operator { value: OperatorValue::If, token: token });
                if_level += 1;
            }
            else if token_type == TokenType::Semicolon {
                let _ = self.stream.consume();
                if self.block_args.len() == 0 {
                    panic!("; present outside of a block");
                }

                *self.block_args.front_mut().unwrap() += 1;
            }
            else if token_type == TokenType::Comma {
                let _ = self.stream.consume();
                if self.func_args.len() == 0 {
                    panic!(", present outside for function argument list");
                }

                *self.func_args.front_mut().unwrap() += 1;
            }
            else if token_type == TokenType::Else {
                let _ = self.stream.consume();
                if_level -= 1;
            }
            else {
                break;
            }
        }

        println!("{:?}", self.output);
        println!("{:?}\n", operators);

        while self.output.len() > 1 && operators.len() > 0{
            let _ = self.generate_operator_expr(operators.pop_front().unwrap())?;
        }

        if self.output.len() == 1 {
            return Ok(self.output.pop_front().unwrap());
        }
        else {
            panic!("Failed to generate single expression\n{:?}\n{:?}", self.output, operators);
        }
    }
}