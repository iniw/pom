pub use insta;

#[macro_export]
macro_rules! snap {
    ($expr:expr) => {
        $crate::snap::insta::with_settings!({prepend_module_to_snapshot => false}, {
            $crate::snap::insta::assert_debug_snapshot!($expr);
        });
    }
}
