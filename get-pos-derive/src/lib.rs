use proc_macro::{self, TokenStream};
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(GetPos)]
pub fn derive(input: TokenStream) -> TokenStream {
    let DeriveInput { ident, generics, .. } = parse_macro_input!(input);
    let tokens = quote! {
        impl #generics GetPos for #ident #generics {
            fn get_pos(&self) -> Pos {
                self.pos
            }
        }
    };
    tokens.into()
}
