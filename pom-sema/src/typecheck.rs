use crate::ir::Ir;

pub struct TypeChecking {}

impl TypeChecking {
    #[expect(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {}
    }

    pub fn check(self, _ir: &mut Ir) {
        todo!()
    }
}
