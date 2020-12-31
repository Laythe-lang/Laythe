extern crate proc_macro;

use quote::quote;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(Trace)]
pub fn derive_trace(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
  // Parse the input tokens into a syntax tree.
  let input = parse_macro_input!(input as DeriveInput);

  // Used in the quasi-quotation below as `#name`.
  let name = input.ident;

  let expanded = quote! {
    impl Trace for #name {
      fn trace(&self) {}
      fn trace_debug(&self, _: &mut dyn Write) {}
    }
  };

  // Hand the output tokens back to the compiler.
  proc_macro::TokenStream::from(expanded)
}
