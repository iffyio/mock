extern crate proc_macro;
use proc_macro::TokenStream;
use proc_macro2::TokenStream as ProcMacro2TokenStream;
use quote::quote;
use syn::spanned::Spanned;
use syn::{FnArg, ReturnType, TraitItem, TraitItemMethod, Type};

fn return_value_field_name_for_method(method: &TraitItemMethod) -> syn::Ident {
    quote::format_ident!("return_value_expect_{}", method.sig.ident.clone())
}

fn return_with_field_name_for_method(method: &TraitItemMethod) -> syn::Ident {
    quote::format_ident!("return_with_expect_{}", method.sig.ident.clone())
}

fn compute_return_with_predicate_type(method: &syn::TraitItemMethod) -> ProcMacro2TokenStream {
    let input_args_type = get_arg_types(method)
        .into_iter()
        .map(|t| quote!(#t))
        .collect::<Vec<_>>();
    let ret_type = get_ret_type(method);
    quote! {
        Box<Fn(( #( #input_args_type ),* )) -> std::option::Option<#ret_type>>
    }
}

fn get_ret_type(method: &syn::TraitItemMethod) -> Type {
    match method.sig.output.clone() {
        syn::ReturnType::Default => syn::parse_quote!(()),
        syn::ReturnType::Type(_, ret_type) => *ret_type,
    }
}

fn get_arg_types(method: &syn::TraitItemMethod) -> Vec<Type> {
    method
        .sig
        .inputs
        .iter()
        .filter_map(|arg| match arg {
            FnArg::Typed(pat_type) => Some(pat_type.ty.clone()),
            FnArg::Receiver(_) => None,
        })
        .map(|t| *t)
        .collect()
}

fn trait_impl_gen(
    trait_name: syn::Ident,
    impl_name: syn::Ident,
    trait_method_decls: Vec<TraitItemMethod>,
) -> ProcMacro2TokenStream {
    let method_impls = trait_method_decls
        .into_iter()
        .map(|method| {
            let return_value_field_name = return_value_field_name_for_method(&method);
            let return_with_field_name = return_with_field_name_for_method(&method);
            let method_name = method.sig.ident.to_string();
            let parameter_names = method
                .sig
                .inputs
                .iter()
                .flat_map(|param| match param {
                    syn::FnArg::Receiver(_) => None,
                    syn::FnArg::Typed(pat) => Some(pat.pat.clone()),
                })
                .collect::<Vec<_>>();
            let method_signature = method.sig;

            quote! {
                #method_signature {
                    use std::borrow::BorrowMut;
                    match *self.#return_value_field_name.borrow_mut() {
                        Some(ref return_value) => {
                            return return_value.clone()
                        },
                        None => {}
                    }
                    match *self.#return_with_field_name.borrow_mut() {
                        Some(ref mut predicate) => {
                            match predicate(( #( #parameter_names ),* )) {
                                Some(value) => return value,
                                None => {}
                            }
                        },
                        None => {}
                    }
                    panic!("No expectation was registered for method '{}'", #method_name)
                }
            }
        })
        .collect::<Vec<_>>();

    quote! {
        impl #trait_name for #impl_name {
            #( #method_impls )*
        }
    }
}

fn compute_mock_impl_fields_identifiers(
    trait_method_decls: &Vec<TraitItemMethod>,
) -> Vec<syn::Ident> {
    trait_method_decls
        .into_iter()
        .flat_map(|method| {
            vec![
                return_value_field_name_for_method(method),
                return_with_field_name_for_method(method),
            ]
        })
        .collect()
}

fn mock_impl_fields_gen(trait_method_decls: Vec<TraitItemMethod>) -> Vec<ProcMacro2TokenStream> {
    trait_method_decls
        .into_iter()
        .map(|method| {
            let ret_type: Box<Type> = match method.sig.output.clone() {
                syn::ReturnType::Default => Box::new(syn::parse_quote!(())),
                syn::ReturnType::Type(_, ret_type) => ret_type,
            };

            let return_value_field_ident = return_value_field_name_for_method(&method);
            let return_with_field_ident = return_with_field_name_for_method(&method);
            let return_with_predicate_type = compute_return_with_predicate_type(&method);
            quote! {
                #return_value_field_ident: std::cell::RefCell<std::option::Option<#ret_type>>,
                #return_with_field_ident: std::cell::RefCell<std::option::Option<#return_with_predicate_type>>
            }
        })
        .collect()
}

fn mock_impl_gen(
    impl_name: syn::Ident,
    trait_method_decls: Vec<TraitItemMethod>,
    field_identifiers: Vec<syn::Ident>,
) -> ProcMacro2TokenStream {
    let methods = trait_method_decls
        .into_iter()
        .map(|method| {
            let comma_separated_types: syn::Type = {
                let expectation_input_arg_types = get_arg_types(&method);
                syn::parse_quote! {
                    ( #( #expectation_input_arg_types ),* )
                }
            };

            let ret_type: Box<Type> = match method.sig.output.clone() {
                syn::ReturnType::Default => Box::new(syn::parse_quote!(())),
                syn::ReturnType::Type(_, ret_type) => ret_type,
            };
            let expectation_type = quote! {
                mock_common::Expectation<#comma_separated_types, #ret_type>
            };
            let span = method.sig.ident.span();

            let method_signature = {
                let lifetime = syn::Lifetime {
                    apostrophe: span.clone(),
                    ident: syn::Ident::new("mock", span.clone()),
                };

                let method_name = quote::format_ident!("expect_{}", method.sig.ident.clone());
                let method_output = ReturnType::Type(
                    syn::token::RArrow {
                        spans: [span.clone(), span.clone()],
                    },
                    Box::new(syn::Type::Verbatim(quote! {
                        impl #expectation_type + #lifetime
                    })),
                );

                let inputs = {
                    let mut inputs = syn::punctuated::Punctuated::new();
                    inputs.push(syn::FnArg::Receiver(syn::Receiver {
                        attrs: vec![],
                        reference: Some((
                            syn::token::And {
                                spans: [span.clone()],
                            },
                            Some(lifetime.clone()),
                        )),
                        mutability: None,
                        self_token: syn::token::SelfValue { span: span.clone() },
                    }));
                    inputs
                };

                let generics = {
                    let mut generics = method.sig.generics.clone();
                    generics.params.insert(
                        0,
                        syn::GenericParam::Lifetime(syn::LifetimeDef {
                            attrs: vec![],
                            lifetime,
                            colon_token: None,
                            bounds: syn::punctuated::Punctuated::new(),
                        }),
                    );
                    generics
                };

                let mut sig = method.sig.clone();
                sig.inputs = inputs;
                sig.ident = method_name;
                sig.output = method_output;
                sig.generics = generics;
                sig
            };

            // given method signature: `fn foo(a: A, b: B, c: C, d: D) -> R;`
            // generate Expectation method impl with the ~signatures:
            //     ```
            //         return_value(self, value: R),
            //         return_with(self, f: FnOnce(arg1: A, arg2: B, arg3: C, arg4: D) -> R)
            //     ```
            fn expectation_methods_gen(method: &syn::TraitItemMethod) -> ProcMacro2TokenStream {
                let return_value_field_name = return_value_field_name_for_method(&method);
                let ret_type = match method.sig.output.clone() {
                    syn::ReturnType::Default => Box::new(syn::parse_quote!(())),
                    syn::ReturnType::Type(_, ret_type) => ret_type,
                };

                let input_args_type = get_arg_types(method)
                    .into_iter()
                    .map(|t| quote!(#t))
                    .collect::<Vec<_>>();
                let return_with_field_name = return_with_field_name_for_method(&method);
                let predicate_type = quote! {
                    Fn(( #( #input_args_type ),* )) -> std::option::Option<#ret_type>
                };
                quote! {
                    fn return_value(self, value: #ret_type) {
                        &self.mock_impl.#return_value_field_name.replace(Some(value));
                    }

                    fn return_with<P>(self, predicate: P)
                    where
                        P: #predicate_type + 'static
                    {
                        &self.mock_impl.#return_with_field_name.replace(
                            Some(Box::new(predicate))
                        );
                    }
                }
            }

            let expectation_methods = expectation_methods_gen(&method);
            quote! {
                #method_signature {
                    struct Expectation<'mock_impl> {
                        mock_impl: &'mock_impl #impl_name
                    }
                    impl<'mock_impl> #expectation_type for Expectation<'mock_impl> {
                        #expectation_methods
                    }
                    Expectation{
                        mock_impl: &self
                    }
                }
            }
        })
        .collect::<Vec<_>>();

    let constructor_decl = {
        quote! {
            pub fn new() -> Self {
                #impl_name {
                    #( #field_identifiers: std::cell::RefCell::new(None) ),*
                }
            }
        }
    };

    quote! {
        impl #impl_name {
            #constructor_decl
            #( #methods )*
        }
    }
}

#[proc_macro_attribute]
pub fn mock_trait(attr: TokenStream, item: TokenStream) -> TokenStream {
    let it = item.clone();
    let trait_def = syn::parse_macro_input!(it as syn::ItemTrait);

    if attr.is_empty() {
        panic!(
            "No mock implementation name was provided for the trait subject '{}'",
            trait_def.ident
        );
    }

    let trait_name = trait_def.ident.clone();
    let mock_impl_name = syn::Ident::new(&attr.to_string(), trait_def.span());

    let trait_method_defs = trait_def
        .items
        .iter()
        .filter_map(|item| match item {
            TraitItem::Method(method) => Some(method.clone()),
            _ => None,
        })
        .collect::<Vec<TraitItemMethod>>();

    let mock_impl_expectation_field_idents =
        compute_mock_impl_fields_identifiers(&trait_method_defs);
    let mock_impl = mock_impl_gen(
        mock_impl_name.clone(),
        trait_method_defs.clone(),
        mock_impl_expectation_field_idents.clone(),
    );
    let trait_impl = trait_impl_gen(
        trait_name,
        mock_impl_name.clone(),
        trait_method_defs.clone(),
    );

    let mock_impl_fields = mock_impl_fields_gen(trait_method_defs.clone());

    let mock_declaration = quote! {
        struct #mock_impl_name {
            #( #mock_impl_fields, )*
        }
    };

    let item: ProcMacro2TokenStream = item.into();
    let tokens = quote! {
        #item
        #mock_declaration
        #mock_impl
        #trait_impl
    };

    TokenStream::from(tokens)
}
