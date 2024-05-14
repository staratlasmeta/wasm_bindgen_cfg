use proc_macro_error::{abort_call_site, proc_macro_error, OptionExt};
use quote::{format_ident, quote, ToTokens};
use syn::parse::Parser;
use syn::punctuated::Punctuated;
use syn::{
    parse_macro_input, Attribute, FnArg, ForeignItem, ImplItem, Item, Meta, Signature, Token,
    TraitItem,
};

#[proc_macro_error]
#[proc_macro_attribute]
pub fn wasm_bindgen_cfg(
    args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let wasm_bindgen_ident = format_ident!("wasm_bindgen");
    let filter_func = |attr: &Attribute| !attr.path().is_ident(&wasm_bindgen_ident);

    let input = parse_macro_input!(input as Item);
    let mut input_no_wasm = input.clone();

    filter_item(&mut input_no_wasm, filter_func);
    let attr_parser = Punctuated::<Meta, Token![,]>::parse_terminated;
    let args: Punctuated<Meta, Token![,]> = attr_parser.parse(args).unwrap();
    let cfg_args = args
        .first()
        .expect_or_abort("Expected at least 1 argument for the cfg attribute")
        .to_token_stream();

    let bindgen_args = args
        .into_iter()
        .skip(1)
        .collect::<Punctuated<Meta, Token![,]>>();

    (quote! {
        #[cfg(#cfg_args)]
        #[::wasm_bindgen::prelude::wasm_bindgen(#bindgen_args)]
        #input

        #[cfg(not(#cfg_args))]
        #input_no_wasm
    })
    .into()
}

fn filter_item(item: &mut Item, filter_func: impl Fn(&Attribute) -> bool + Copy) {
    match item {
        Item::Const(c) => c.attrs.retain(filter_func),
        Item::ExternCrate(ec) => ec.attrs.retain(filter_func),
        Item::ForeignMod(fm) => {
            fm.attrs.retain(filter_func);
            for item in &mut fm.items {
                match item {
                    ForeignItem::Fn(f) => {
                        f.attrs.retain(filter_func);
                        filter_sig(&mut f.sig, filter_func);
                    }
                    ForeignItem::Static(s) => s.attrs.retain(filter_func),
                    ForeignItem::Type(t) => t.attrs.retain(filter_func),
                    ForeignItem::Macro(m) => m.attrs.retain(filter_func),
                    ForeignItem::Verbatim(_) => {}
                    _ => abort_call_site!("Unknown ForeignItem"),
                }
            }
        }
        Item::Macro(m) => m.attrs.retain(filter_func),
        Item::Mod(m) => {
            m.attrs.retain(filter_func);
            for item in m.content.as_mut().map(|(_, c)| c).into_iter().flatten() {
                filter_item(item, filter_func)
            }
        }
        Item::Static(s) => s.attrs.retain(filter_func),
        Item::TraitAlias(ta) => ta.attrs.retain(filter_func),
        Item::Type(t) => t.attrs.retain(filter_func),
        Item::Union(u) => {
            u.attrs.retain(filter_func);
            for field in &mut u.fields.named {
                field.attrs.retain(filter_func)
            }
        }
        Item::Use(u) => u.attrs.retain(filter_func),
        Item::Verbatim(_) => {}
        Item::Enum(e) => {
            e.attrs.retain(filter_func);
            for variant in &mut e.variants {
                variant.attrs.retain(filter_func);
                for field in &mut variant.fields {
                    field.attrs.retain(filter_func);
                }
            }
        }
        Item::Fn(func) => {
            func.attrs.retain(filter_func);
            filter_sig(&mut func.sig, filter_func)
        }
        Item::Impl(i) => {
            i.attrs.retain(filter_func);
            for item in &mut i.items {
                match item {
                    ImplItem::Const(c) => c.attrs.retain(filter_func),
                    ImplItem::Fn(func) => {
                        func.attrs.retain(filter_func);
                        filter_sig(&mut func.sig, filter_func)
                    }
                    ImplItem::Type(t) => t.attrs.retain(filter_func),
                    ImplItem::Macro(m) => m.attrs.retain(filter_func),
                    ImplItem::Verbatim(_) => {}
                    _ => abort_call_site!("Unknown ImplItem"),
                }
            }
        }
        Item::Struct(s) => {
            s.attrs.retain(filter_func);
            for field in &mut s.fields {
                field.attrs.retain(filter_func)
            }
        }
        Item::Trait(t) => {
            t.attrs.retain(filter_func);
            for item in &mut t.items {
                match item {
                    TraitItem::Const(c) => c.attrs.retain(filter_func),
                    TraitItem::Fn(f) => {
                        f.attrs.retain(filter_func);
                        filter_sig(&mut f.sig, filter_func);
                    }
                    TraitItem::Type(t) => t.attrs.retain(filter_func),
                    TraitItem::Macro(m) => m.attrs.retain(filter_func),
                    TraitItem::Verbatim(_) => {}
                    _ => abort_call_site!("Unknown TraitItem"),
                }
            }
        }
        _ => abort_call_site!("Unknown Item"),
    }
}

fn filter_sig(sig: &mut Signature, filter_func: impl Fn(&Attribute) -> bool + Copy) {
    for input in &mut sig.inputs {
        match input {
            FnArg::Receiver(r) => &mut r.attrs,
            FnArg::Typed(t) => &mut t.attrs,
        }
        .retain(filter_func);
    }
    if let Some(v) = &mut sig.variadic {
        v.attrs.retain(filter_func)
    }
}
