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
        UnexpectedToken
    }
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Operator<'a> {
    Op(Op),
    Function(&'a str),
    Paren,
    Block,
    If
}

pub struct Parser<'a, T: IntoIterator<Item = Token<'a>>> {
    stream: TokenStream<'a, T>,
    module: Module,

    func_args: VecDeque<usize>, //keeps track of arguments passed to a function
    block_args: VecDeque<usize> //keeps track of exprs passed to a block
}

impl<'a, T: IntoIterator<Item = Token<'a>>> Parser<'a, T> {
    pub fn new(stream: TokenStream<'a, T> ) -> Parser<'a, T> {
        Parser {
            stream: stream,
            module: Module::new(),
            func_args: VecDeque::new(),
            block_args: VecDeque::new()
        }
    }

    pub fn parse(mut self) -> Result<Module, UnexpectedToken<'a>> {
        while let Some(token_type) = self.stream.peek_token_type() {
            match token_type {
                TokenType::Function => self.parse_function_def()?,
                _ => ()
            }
        }

        Ok(self.module)
    }

    fn parse_function_def(&mut self) -> Result<(), UnexpectedToken<'a>> {
        let _ = self.stream.expect_function()?;
        let name = self.stream.expect_identifier()?;
        let _ = self.stream.expect_lparen();

        let mut args: Vec<&str> = Vec::new();
        loop {
            let res = self.stream.consume();
            if res.is_none() {
                //Ran out of input
                return Err(UnexpectedToken::single(TokenType::Identifier, Token::eof()));
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
                    return Err(UnexpectedToken::multiple(&[TokenType::Identifier, TokenType::RParen], token_res));
                }
            }
        }

        let expr = self.parse_expr()?;

        let def = Definition::Function(String::from(name), args.into_iter().map(String::from).collect(), expr);
        self.module.definitions.push(def);

        Ok(())
    }

    fn generate_operator_expr(&mut self, output: &mut VecDeque<Box<Expr>>, operator: Operator) -> bool {
        if let Operator::Op(op) = operator {
            if op.is_unary() {
                if output.len() >= 1 {
                    let expr = output.pop_back().unwrap();
                    let unary_op = UnaryOp::from_op(op);
                    let new_expr = Box::new(Expr::UnaryOp(unary_op, expr));
                    output.push_back(new_expr);
                    true
                }
                else {
                    false
                }
            }
            else {
                if output.len() >= 2 {
                    let rhs = output.pop_back().unwrap();
                    let lhs = output.pop_back().unwrap();
                    let bin_op = BinOp::from_op(op);
                    let new_expr = Box::new(Expr::BinOp(bin_op, lhs, rhs));
                    output.push_back(new_expr);
                    true
                }
                else {
                    false
                }
            }
        }
        else if let Operator::Function(iden) = operator {
            //Siphon all output to be arguments to the function
            let arg_count = self.func_args.pop_front().unwrap() + 1;

            let mut args = Vec::new();
            for _ in 0..arg_count {
                args.push(output.pop_back().unwrap());
            }

            let expr = Box::new(Expr::FunctionApp(String::from(iden), args));
            output.push_back(expr);

            true
        }
        else if operator == Operator::Block {
             //Siphon off any remaining output into this block
            let block_count = self.block_args.pop_back().unwrap() + 1;

             let mut exprs = Vec::new();
             for _ in 0..block_count {
                exprs.push(output.pop_back().unwrap());
            }

            let expr = Box::new(Expr::Block(exprs));
            output.push_back(expr);
            true
        }
        else if operator == Operator::If {
            if output.len() >= 3 {
                let else_expr = output.pop_back().unwrap();
                let then_expr = output.pop_back().unwrap();
                let cond_expr = output.pop_back().unwrap();

                let expr = Box::new(Expr::If(IfExpr {
                    cond: cond_expr,
                    then_expr: then_expr,
                    else_expr: else_expr
                }));

                output.push_back(expr);
                true
            }
            else {
                false
            }
        }
        else {
            panic!("Unsupported operator {:?}", operator)
        }
    }

    fn parse_expr(&mut self) -> Result<Box<Expr>, UnexpectedToken<'a>> {
        let mut output: VecDeque<Box<Expr>> = VecDeque::new();
        let mut operators: VecDeque<Operator<'a>> = VecDeque::new();
        let mut if_level = 0;

        while let Some(token_type) = self.stream.peek_token_type() {
            println!("{:?}", output);
            println!("{:?}\n", operators);

            if token_type == TokenType::Int {
                let value = self.stream.consume_int();
                output.push_back(Box::new(Expr::ConstInt(value)));

                if if_level > 0 {
                    while let Some(op) = operators.pop_front() {
                        if op != Operator::If && op != Operator::Block && op != Operator::Paren {
                            if !self.generate_operator_expr(&mut output, op) {
                                panic!("Failed to generate operator expression");
                            }
                        }
                        else {
                            operators.push_front(op);
                            break;
                        }
                    }
                }
            }
            else if token_type == TokenType::Identifier {
                let iden = self.stream.consume_identifier();

                if let Some(next_type) = self.stream.peek_token_type() {
                    if next_type == TokenType::LParen {
                        //function application
                        let _ = self.stream.consume();
                        operators.push_front(Operator::Function(iden));
                        self.func_args.push_front(0);
                    }
                    else {
                        //Just treat this as a variable for now
                        //TODO: make this check the next token
                        output.push_back(Box::new(Expr::Var(String::from(iden))));

                        if if_level > 0 {
                            while let Some(op) = operators.pop_front() {
                                if op != Operator::If {
                                    if !self.generate_operator_expr(&mut output, op) {
                                        panic!("Failed to generate operator expression");
                                    }
                                }
                                else {
                                    operators.push_front(op);
                                    break;
                                }
                            }
                        }
                    }
                }
                else {
                    //No next token, just treat this an a variable
                    output.push_back(Box::new(Expr::Var(String::from(iden))));

                    if if_level > 0 {
                        while let Some(op) = operators.pop_front() {
                            if op != Operator::If {
                                if !self.generate_operator_expr(&mut output, op) {
                                    panic!("Failed to generate operator expression");
                                }
                            }
                            else {
                                operators.push_front(op);
                                break;
                            }
                        }
                    }
                }
            }
            else if token_type == TokenType::Operator {
                let op = self.stream.consume_op();

                while operators.len() > 0 {
                    let front = *operators.front().unwrap();
                    if let Operator::Op(top) = front {
                        let op_prec = op.precedence();
                        let op_assoc = op.associativity();
                        let top_prec = top.precedence();

                        if top_prec < op_prec || (op_prec == top_prec && op_assoc == OpAssociativity::Left) {
                            let op = operators.pop_front().unwrap();
                            if !self.generate_operator_expr(&mut output, op) {
                                panic!("Failed to generate operator expression");
                            }
                        }
                        else {
                            break;
                        }
                    }
                    else if front == Operator::Paren || front == Operator::Block || front == Operator::If {
                       break;
                    }
                    else if let Operator::Function(_) = front {
                        break;
                    }
                    else {
                        panic!("Unsupported operator {:?}", operators.front().unwrap());
                    }
                }

                operators.push_front(Operator::Op(op));
            }
            else if token_type == TokenType::LParen {
                let _ = self.stream.consume();
                operators.push_front(Operator::Paren);
            }
            else if token_type == TokenType::RParen {
                let _ = self.stream.consume();
                if operators.len() == 0 {
                    panic!("Mismatched parenthesis");
                }
                
                let mut found_paren = false;
                while let Some(op) = operators.pop_front() {
                    if op == Operator::Paren {
                        found_paren = true;
                        break;
                    }
                    else if let Operator::Function(_) = op {
                        found_paren = true;

                        if !self.generate_operator_expr(&mut output, op) {
                            panic!("Failed to generate operator expression");
                        }
                        break;
                    }
                    else {
                        if !self.generate_operator_expr(&mut output, op) {
                            panic!("Failed to generate operator expression");
                        }
                    }
                }

                if !found_paren {
                    panic!("Mismatched parenthesis");
                }
            }
            else if token_type == TokenType::LBrace {
                let _ = self.stream.consume();
                operators.push_front(Operator::Block);
                self.block_args.push_back(0);
            }
            else if token_type == TokenType::RBrace {
                let _ = self.stream.consume();
                if operators.len() == 0 {
                    panic!("Mismatched braces");
                }
                
                let mut found_brace = false;
                while let Some(op) = operators.pop_front() {
                    println!("{:?}", op);
                    if op == Operator::Block {
                        found_brace = true;

                        if !self.generate_operator_expr(&mut output, op) {
                            panic!("Failed to generate operator expression");
                        }

                        break;
                    }
                    else {
                        if !self.generate_operator_expr(&mut output, op) {
                            panic!("Failed to generate operator expression");
                        }
                    }
                }

                if !found_brace {
                    panic!("Mismatched braces");
                }
            }
            else if token_type == TokenType::If {
                let _ = self.stream.consume();
                operators.push_front(Operator::If);
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

        println!("{:?}", output);
        println!("{:?}\n", operators);

        while output.len() > 1 && operators.len() > 0{
            if !self.generate_operator_expr(&mut output, operators.pop_front().unwrap()) {
                break;
            }
        }

        if output.len() == 1 {
            return Ok(output.pop_front().unwrap());
        }
        else {
            panic!("Failed to generate single expression\n{:?}\n{:?}", output, operators);
        }
    }
}