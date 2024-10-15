use crate::{
    pool::{Handle, Pool},
    syn::{BinaryOp, Expr, Literal, Stmt},
};

pub struct Interpreter {}

#[allow(clippy::only_used_in_recursion)]
impl Interpreter {
    pub fn new() -> Self {
        Self {}
    }

    pub fn evaluate(&mut self, stmts: Pool<Stmt>, exprs: Pool<Expr>) -> Result<(), RuntimeError> {
        for stmt in &stmts {
            self.execute_statement(&stmts, &exprs, stmt)?;
        }
        Ok(())
    }

    fn execute_statement(
        &mut self,
        stmts: &Pool<Stmt>,
        exprs: &Pool<Expr>,
        stmt: Handle<Stmt>,
    ) -> Result<(), RuntimeError> {
        match stmts.get(stmt) {
            Stmt::Expr(expr) => {
                dbg!(self.evaluate_expression(stmts, exprs, *expr)?);
            }
            Stmt::Empty => (),
        };
        Ok(())
    }

    fn evaluate_expression(
        &mut self,
        stmts: &Pool<Stmt>,
        exprs: &Pool<Expr>,
        expr: Handle<Expr>,
    ) -> Result<Value, RuntimeError> {
        match exprs.get(expr) {
            Expr::Binary { left, op, right } => {
                let left = self.evaluate_expression(stmts, exprs, *left)?;
                let right = self.evaluate_expression(stmts, exprs, *right)?;
                match op {
                    BinaryOp::Add => left.add(right),
                    BinaryOp::Sub => left.sub(right),
                    BinaryOp::Div => left.div(right),
                    BinaryOp::Mul => left.mul(right),
                }
            }
            Expr::Literal(literal) => match literal {
                Literal::Number(number) => Ok(Value::Number(*number)),
            },
        }
    }
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Number(f64),
}

impl Value {
    fn add(mut self, right: Value) -> Result<Value, RuntimeError> {
        self.add_assign(right)?;
        Ok(self)
    }

    fn add_assign(&mut self, right: Value) -> Result<(), RuntimeError> {
        use Value::*;
        match (&mut *self, right) {
            (Number(a), Number(b)) => *a += b,
        };
        Ok(())
    }

    fn sub(mut self, right: Value) -> Result<Value, RuntimeError> {
        self.sub_assign(right)?;
        Ok(self)
    }

    fn sub_assign(&mut self, right: Value) -> Result<(), RuntimeError> {
        use Value::*;
        match (&mut *self, right) {
            (Number(a), Number(b)) => *a -= b,
        };
        Ok(())
    }

    fn div(mut self, right: Value) -> Result<Value, RuntimeError> {
        self.div_assign(right)?;
        Ok(self)
    }

    fn div_assign(&mut self, right: Value) -> Result<(), RuntimeError> {
        use Value::*;
        match (&mut *self, right) {
            (Number(a), Number(b)) => *a /= b,
        };
        Ok(())
    }

    fn mul(mut self, right: Value) -> Result<Value, RuntimeError> {
        self.mul_assign(right)?;
        Ok(self)
    }

    fn mul_assign(&mut self, right: Value) -> Result<(), RuntimeError> {
        use Value::*;
        match (&mut *self, right) {
            (Number(a), Number(b)) => *a *= b,
        };
        Ok(())
    }
}

#[derive(Debug, Copy, Clone)]
pub enum RuntimeError {}
