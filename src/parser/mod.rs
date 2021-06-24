pub mod lexer;
pub mod token;
pub mod ast;

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
        TokenStream,
        TokenType,
        UnexpectedToken,
        Op,
        OpAssociativity
    }
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Operator<'a> {
    Op(Op),
    Function(&'a str),
    Paren,
    Brace,
    If
}

pub struct Parser<'a, T: IntoIterator<Item = Token<'a>>> {
    stream: TokenStream<'a, T>,
    module: Module
}

impl<'a, T: IntoIterator<Item = Token<'a>>> Parser<'a, T> {
    pub fn new(stream: TokenStream<'a, T> ) -> Parser<'a, T> {
        Parser {
            stream: stream,
            module: Module::new()
        }
    }

    pub fn parse(mut self) -> Result<Module, UnexpectedToken> {
        while let Some(token_type) = self.stream.peek_token_type() {
            match token_type {
                TokenType::Function => self.parse_function_def()?,
                _ => ()
            }
        }

        Ok(self.module)
    }

    fn parse_function_def(&mut self) -> Result<(), UnexpectedToken> {
        let _ = self.stream.expect_function()?;
        let name = self.stream.expect_identifier()?;
        let _ = self.stream.expect_lparen();

        let mut args: Vec<&str> = Vec::new();
        loop {
            let res = self.stream.consume();
            if res.is_none() {
                //Ran out of input
                return Err(UnexpectedToken::single(TokenType::Identifier, TokenType::Eof));
            }
            else {
                let token = res.unwrap();
                if token == Token::RParen {
                    //args list finished
                    break;
                }
                else if let Token::Identifier(iden) = token {
                    args.push(iden);
                    if let Some(TokenType::Comma) = self.stream.peek_token_type() {
                        //Consume the comma if there is one
                        //If the next token isn't an identifier or comma, the next iteration will catch it
                        let _ = self.stream.consume();
                    }
                }
                else {
                    return Err(UnexpectedToken::multiple(&[TokenType::Identifier, TokenType::RParen], token.token_type()));
                }
            }
        }

        let expr = self.parse_expr()?;

        let def = Definition::Function(String::from(name), args.into_iter().map(String::from).collect(), expr);
        self.module.definitions.push(def);

        Ok(())
    }

    fn generate_operator_expr(output: &mut VecDeque<Box<Expr>>, func_args: &mut VecDeque<usize>, operator: Operator) -> bool {
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
            let arg_count = func_args.pop_front().unwrap() + 1;

            let mut args = Vec::new();
            for i in 0..arg_count {
                args.push(output.pop_back().unwrap());
            }

            let expr = Box::new(Expr::FunctionApp(String::from(iden), args));
            output.push_back(expr);

            true
        }
        else if operator == Operator::Brace {
             //Siphon off any remaining output into this block
             let mut exprs = Vec::new();
             while let Some(expr) = output.pop_front() {
                exprs.push(expr);
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

    fn parse_expr(&mut self) -> Result<Box<Expr>, UnexpectedToken> {
        let mut output: VecDeque<Box<Expr>> = VecDeque::new();
        let mut operators: VecDeque<Operator<'a>> = VecDeque::new();
        let mut func_args: VecDeque<usize> = VecDeque::new();
        let mut brace_level = 0;
        let mut function_level = 0;
        let mut if_level = 0;

        while let Some(token_type) = self.stream.peek_token_type() {
            println!("{:?}", output);
            println!("{:?}\n", operators);

            if token_type == TokenType::Int {
                let value = self.stream.consume_int();
                output.push_back(Box::new(Expr::ConstInt(value)));
            }
            else if token_type == TokenType::Identifier {
                let iden = self.stream.consume_identifier();

                if let Some(next_type) = self.stream.peek_token_type() {
                    if next_type == TokenType::LParen {
                        //function application
                        let _ = self.stream.consume();
                        function_level += 1;
                        operators.push_front(Operator::Function(iden));
                        func_args.push_front(0);
                    }
                    else {
                        //Just treat this as a variable for now
                        //TODO: make this check the next token
                        output.push_back(Box::new(Expr::Var(String::from(iden))));
                    }
                }
                else {
                    //No next token, just treat this an a variable
                    output.push_back(Box::new(Expr::Var(String::from(iden))));
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
                            if !Self::generate_operator_expr(&mut output, &mut func_args, op) {
                                panic!("Failed to generate operator expression");
                            }
                        }
                        else {
                            break;
                        }
                    }
                    else if front == Operator::Paren || front == Operator::Brace || front == Operator::If {
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
                    else if let Operator::Function(iden) = op {
                        found_paren = true;
                        function_level -= 1;

                        if !Self::generate_operator_expr(&mut output, &mut func_args, op) {
                            panic!("Failed to generate operator expression");
                        }
                        break;
                    }
                    else {
                        if !Self::generate_operator_expr(&mut output, &mut func_args, op) {
                            panic!("Failed to generate operator expression");
                        }
                    }
                }

                if !found_paren {
                    panic!("Mismatched parenthesis");
                }
            }
            else if token_type == TokenType::LCBracket {
                let _ = self.stream.consume();
                operators.push_front(Operator::Brace);
                brace_level += 1;
            }
            else if token_type == TokenType::RCBracket {
                let _ = self.stream.consume();
                if operators.len() == 0 {
                    panic!("Mismatched braces");
                }
                
                let mut found_brace = false;
                while let Some(op) = operators.pop_front() {
                    if op == Operator::Brace {
                        found_brace = true;

                        if !Self::generate_operator_expr(&mut output, &mut func_args, op) {
                            panic!("Failed to generate operator expression");
                        }

                        break;
                    }
                    else {
                        if !Self::generate_operator_expr(&mut output, &mut func_args, op) {
                            panic!("Failed to generate operator expression");
                        }
                    }
                }

                if !found_brace {
                    panic!("Mismatched braces");
                }

                brace_level -= 1;
            }
            else if token_type == TokenType::If {
                let _ = self.stream.consume();
                operators.push_front(Operator::If);
                if_level += 1;
            }
            else if token_type == TokenType::Semicolon {
                let _ = self.stream.consume();
                if brace_level == 0 {
                    panic!("; present outside of a block");
                }
            }
            else if token_type == TokenType::Comma {
                let _ = self.stream.consume();
                if function_level == 0 {
                    panic!(", present outside for function argument list");
                }

                *func_args.front_mut().unwrap() += 1;
            }
            else if token_type == TokenType::Else {
                let _ = self.stream.consume();
                if_level -= 1;

                while let Some(operator) = operators.pop_front() {
                    if operator == Operator::If {
                        break;
                    }
                    else {
                        if !Self::generate_operator_expr(&mut output, &mut func_args, operator) {
                            panic!("Failed to generate operator expression");
                        }   
                    }
                }
            }
            else {
                break;
            }
        }

        println!("{:?}", output);
        println!("{:?}\n", operators);

        while output.len() > 1 && operators.len() > 0{
            if !Self::generate_operator_expr(&mut output, &mut func_args, operators.pop_front().unwrap()) {
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