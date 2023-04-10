extern crate proc_macro;
use proc_macro::TokenStream;
use quote::{quote, quote_spanned};


#[proc_macro]
pub fn get_ders(input: TokenStream) -> TokenStream {
    use proc_macro2::TokenTree;

    let input: proc_macro2::TokenStream = input.into();
    let mut iter = input.into_iter();
    let term_name;
    let mut tms = Vec::new();
    let nonterm_name;
    let mut nontms = Vec::new();
    let mut ders = Vec::new();

    if let Some(TokenTree::Ident(ident)) = iter.next() {
        term_name = ident;
    } else {
        panic!("Invalid token");
    }
    iter.next();
    if let Some(TokenTree::Ident(ident)) = iter.next() {
        nonterm_name = ident;
    } else {
        panic!("Invalid token");
    }
    iter.next();

    while let Some(token) = iter.next() {
        match token {
            TokenTree::Ident(ident) => {
                tms.push(ident.to_string());
            }
            TokenTree::Punct(punct) => {
                if punct.as_char() == ',' {
                    // Commas separate idents
                } else if punct.as_char() == ';' {
                    // Semicolon marks the end of the list
                    break;
                } else {
                    let span = punct.span().into();
                    return quote_spanned!(span=> compile_error!("Unexpected Punctuation")).into();
                }
            }
            _ => {
                panic!("Unexpected token");
            }
        }
    }
    while let Some(token) = iter.next() {
        match token {
            TokenTree::Ident(ident) => {
                nontms.push(ident.to_string());
            }
            TokenTree::Punct(punct) => {
                if punct.as_char() == ',' {
                    // Commas separate idents
                } else if punct.as_char() == ';' {
                    // Semicolon marks the end of the list
                    break;
                } else {
                    let span = punct.span().into();
                    return quote_spanned!(span=> compile_error!("Unexpected Punctuation")).into();
                }
            }
            _ => {
                panic!("Unexpected token");
            }
        }
    }

    // Output should be formatted like: Der{ from: Start,    to: vec![NonTm(Stm), Tm(EOF)] },
    let mut lhs;
    let mut der = quote!{};
    if let Some(TokenTree::Ident(l)) = iter.next() {
        lhs = l;
    } else {
        panic!("Missing Derivation");
    }
    while let Some(token) = iter.next() {
        match token {
            TokenTree::Ident(ident) => {
                if tms.contains(&ident.to_string()) {
                    der = quote!{
                        #der grammar::Symbol::Tm(#term_name::#ident),
                    }
                } else if nontms.contains(&ident.to_string()) {
                    der = quote!{
                        #der grammar::Symbol::NonTm(#nonterm_name::#ident),
                    }
                }
            }
            TokenTree::Punct(punct) => {
                if punct.as_char() == ',' {
                    // Commas separate idents
                } else if punct.as_char() == ';' {
                    // Semicolon marks the end of the der
                    // Store and continue
                    ders.push((der, lhs));
                    der = quote!{};
                    if let Some(TokenTree::Ident(l)) = iter.next() {
                        lhs = l;
                    } else {
                        panic!("Missing Derivation");
                    }
                } else {
                    let span = punct.span().into();
                    return quote_spanned!(span=> compile_error!("Unexpected Punctuation")).into();
                }
            }
            _ => {
                panic!("Unexpected token");
            }
        }
    }
    let ders = ders
            .into_iter()
            .map(|(der, lhs)| {
                    quote!{
                        grammar::Der{ from: #nonterm_name::#lhs, to: vec![#der] }
                    }
                }
            );
    let mut new_ders = Vec::new();
    for der in ders {
        new_ders.push(der);
    }
    let ders = new_ders;
    println!("ders.len(): {}", ders.len());
    let retval = quote!{
        vec![#(#ders),*]
    };

    retval.into()
}



