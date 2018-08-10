use std::borrow::BorrowMut;
use std::marker::PhantomData;

use structure::types::*;
use structure::modules::*;
use structure::instructions::*;

use crate::runtime_structure::*;
use crate::numerics::*;
use crate::structure_references::*;

// TODO: More central location
pub struct ExecCtx<Ref, Sto, Stk> {
    pub store: Sto,
    pub stack: Stk,
    _marker: PhantomData<Ref>,
}

impl<Ref, Sto, Stk> ExecCtx<Ref, Sto, Stk>
    where Sto: BorrowMut<Store<Ref>>,
          Stk: BorrowMut<Stack>,
          Ref: StructureReference,
{
    pub fn new(store: Sto, stack: Stk) -> Self {
        ExecCtx {
            store,
            stack,
            _marker: PhantomData,
        }
    }

    pub fn store(&mut self) -> &mut Store<Ref> {
        self.store.borrow_mut()
    }

    pub fn stack(&mut self) -> &mut Stack {
        self.stack.borrow_mut()
    }

    pub fn evaluate_expr(&mut self, expr: &Expr) -> Val {

        // TODO: execute expr.body

        let v = self.stack.borrow_mut().pop_val();
        v
    }


}
