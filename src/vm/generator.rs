use crate::{
    lex::span::Spanned,
    pool::{Handle, Pool},
    syn::ast::{BinaryOp, Expr, Literal, Stmt, SymbolDecl, SymbolInfo, VarInfo},
};

use super::{
    Op::{self, *},
    Word,
    error::{Error, ErrorKind},
};

pub mod env;

use env::{EnvManager, Symbol};

pub struct Generator<'syn> {
    program: Vec<Op>,
    envs: EnvManager<'syn>,
    function_generation_queue: Vec<SymbolDecl<'syn>>,
}

impl<'syn> Generator<'syn> {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            program: Vec::new(),
            envs: EnvManager::with_global_env(),
            function_generation_queue: Vec::new(),
        }
    }

    pub fn generate(
        mut self,
        global_stmts: &'syn Vec<Handle<Spanned<Stmt<'syn>>>>,
        stmts: &'syn Pool<Spanned<Stmt<'syn>>>,
        exprs: &'syn Pool<Spanned<Expr<'syn>>>,
    ) -> Result<Vec<Op>, Error<'syn>> {
        // The address here is the entry point, which will be patched up later when we find the
        // `main` symbol.
        self.program = vec![Call { addr: 0 }, Halt];

        for stmt in global_stmts {
            let Spanned(stmt_data, span) = &stmts[*stmt];
            match stmt_data {
                Stmt::SymbolDecl(SymbolDecl {
                    identifier,
                    info: SymbolInfo::Fn(body),
                }) => {
                    self.queue_function_generation(stmts, identifier, *stmt, *body)?;
                }
                Stmt::SymbolDecl(SymbolDecl {
                    // 'main' symbol in the outer scope that isn't a function.
                    identifier: "main",
                    info: _,
                }) => return Err(Error(Spanned(ErrorKind::InvalidMainDeclaration, *span))),

                _ => return Err(Error(Spanned(ErrorKind::NotInsideFunction, *span))),
            }

            while let Some(SymbolDecl {
                identifier,
                info: SymbolInfo::Fn(body),
            }) = self.function_generation_queue.pop()
            {
                let addr = self.generate_function(stmts, exprs, identifier, body)?;
                if identifier == "main" {
                    self.program[0] = Call { addr };
                }
            }
        }

        Ok(self.program)
    }

    fn generate_statement(
        &mut self,
        stmts: &Pool<Spanned<Stmt<'syn>>>,
        exprs: &Pool<Spanned<Expr<'syn>>>,
        stmt: Handle<Spanned<Stmt<'syn>>>,
    ) -> Result<(), Error<'syn>> {
        let Spanned(stmt_data, span) = &stmts[stmt];
        match stmt_data {
            Stmt::Expr(expr) => {
                self.generate_expression(stmts, exprs, *expr)?;
            }
            Stmt::SymbolDecl(SymbolDecl { identifier, info }) => match info {
                SymbolInfo::Fn(body) => {
                    self.queue_function_generation(stmts, identifier, stmt, *body)?;
                }
                SymbolInfo::Var(VarInfo::Value(expr)) => {
                    let reg = self.generate_expression(stmts, exprs, *expr)?;

                    self.envs
                        .declare_symbol(identifier, Symbol::Variable(reg))
                        .ok_or(Error(Spanned(
                            ErrorKind::InvalidShadowing(identifier),
                            *span,
                        )))?;
                }
                _ => unimplemented!(),
            },
            Stmt::Print(expr) => {
                let reg = self.generate_expression(stmts, exprs, *expr)?;
                self.program.push(Print { reg });
            }
        }
        Ok(())
    }

    fn generate_expression(
        &mut self,
        stmts: &Pool<Spanned<Stmt<'syn>>>,
        exprs: &Pool<Spanned<Expr<'syn>>>,
        expr: Handle<Spanned<Expr<'syn>>>,
    ) -> Result<u8, Error<'syn>> {
        let Spanned(expr_data, span) = &exprs[expr];
        match expr_data {
            Expr::Binary { lhs, op, rhs } => {
                let dst = self.allocate_reg();
                let left = self.generate_expression(stmts, exprs, *lhs)?;
                let right = self.generate_expression(stmts, exprs, *rhs)?;
                let op = match op {
                    BinaryOp::Add => Add { dst, left, right },
                    BinaryOp::Sub => Sub { dst, left, right },
                    BinaryOp::Div => Div { dst, left, right },
                    BinaryOp::Mul => Mul { dst, left, right },
                };
                self.program.push(op);
                Ok(dst)
            }
            Expr::Block(block_stmts) => {
                self.envs.push();

                for stmt in &block_stmts[0..block_stmts.len() - 1] {
                    self.generate_statement(stmts, exprs, *stmt)?;
                }

                let dst;
                if let Some(last_stmt) = block_stmts.last() {
                    if let Spanned(Stmt::Expr(expr), _) = stmts[*last_stmt] {
                        dst = self.generate_expression(stmts, exprs, expr)?;
                    } else {
                        self.generate_statement(stmts, exprs, *last_stmt)?;
                        dst = self.allocate_reg();
                    }
                } else {
                    dst = self.allocate_reg();
                }

                self.envs.pop();

                Ok(dst)
            }
            Expr::Call(expr) => match &exprs[*expr].0 {
                Expr::Symbol(identifier) => match self
                    .envs
                    .find_symbol(identifier)
                    .ok_or(Error(Spanned(ErrorKind::UndeclaredSymbol, *span)))?
                {
                    Symbol::Function(addr) => {
                        let addr = *addr;
                        // TODO: Populate this register with the return value.
                        let dst = self.allocate_reg();
                        self.program.push(Call { addr });
                        Ok(dst)
                    }
                    _ => unimplemented!(),
                },
                _ => unimplemented!(),
            },
            Expr::Literal(literal) => match literal {
                Literal::Number(number) => {
                    let dst = self.allocate_reg();
                    self.program.push(LoadImm { dst, imm: *number });
                    Ok(dst)
                }
            },
            Expr::Symbol(identifier) => match self
                .envs
                .find_symbol(identifier)
                .ok_or(Error(Spanned(ErrorKind::UndeclaredSymbol, *span)))?
            {
                Symbol::Variable(reg) => Ok(*reg),
                _ => Err(Error(Spanned(ErrorKind::SymbolIsNotVariable, *span))),
            },
        }
    }

    fn generate_function(
        &mut self,
        stmts: &Pool<Spanned<Stmt<'syn>>>,
        exprs: &Pool<Spanned<Expr<'syn>>>,
        identifier: &'syn str,
        body: Handle<Spanned<Expr<'syn>>>,
    ) -> Result<Word, Error<'syn>> {
        let prologue_addr = self.generate_prologue();

        self.envs.push_function_frame(identifier);

        self.generate_expression(stmts, exprs, body)?;

        self.generate_epilogue();
        self.patch_prologue(prologue_addr);

        self.envs.pop_function_frame();

        if let Some(Symbol::Function(addr)) = self.envs.find_symbol(identifier) {
            *addr = prologue_addr;
        }

        Ok(prologue_addr as Word)
    }

    fn queue_function_generation(
        &mut self,
        stmts: &Pool<Spanned<Stmt<'syn>>>,
        identifier: &'syn str,
        decl: Handle<Spanned<Stmt<'syn>>>,
        body: Handle<Spanned<Expr<'syn>>>,
    ) -> Result<(), Error<'syn>> {
        // FIXME: Encode the fact that this function is not yet completed somehow,
        //        then generate `Call`s to it in a special way, maybe a `ToBePatchedCall` opcode or
        //        something, then iterate the active function frame when this function is
        //        eventually generated and patch those special opcodes with the correct address.
        self.envs
            .declare_symbol(identifier, Symbol::Function(0))
            .ok_or(Error(Spanned(
                ErrorKind::InvalidShadowing(identifier),
                stmts[decl].span(),
            )))?;

        self.function_generation_queue.push(SymbolDecl {
            identifier,
            info: SymbolInfo::Fn(body),
        });

        Ok(())
    }

    #[inline(always)]
    fn generate_prologue(&mut self) -> Word {
        // This dummy value of `0` will be patched up later.
        self.program.push(Reserve { num_regs: 0 });
        (self.program.len() - 1) as Word
    }

    #[inline(always)]
    fn generate_epilogue(&mut self) {
        self.program.push(Ret);
    }

    #[inline(always)]
    fn patch_prologue(&mut self, prologue_addr: Word) {
        // Update the previously added `Reserve` op with the number of allocated registers
        self.program[prologue_addr as usize] = Reserve {
            num_regs: self.envs.active_function_frame().num_registers,
        };
    }

    #[inline(always)]
    fn allocate_reg(&mut self) -> u8 {
        let count = &mut self.envs.active_function_frame().num_registers;
        std::mem::replace(count, *count + 1)
    }
}
