use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::{quote, ToTokens};
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
    token::Comma,
    Data, DataStruct, DeriveInput, Meta,
};

use crate::{
    bevy_ecs_path,
    world_query::{self, item_struct, world_query_impl, FieldsInfo, NamesInfo},
};

#[derive(Default)]
struct WorldQueryDataAttributes {
    pub is_mutable: bool,

    pub derive_args: Punctuated<syn::Meta, syn::token::Comma>,
}

static MUTABLE_ATTRIBUTE_NAME: &str = "mutable";
static DERIVE_ATTRIBUTE_NAME: &str = "derive";

mod field_attr_keywords {
    syn::custom_keyword!(ignore);
}

pub static WORLD_QUERY_DATA_ATTRIBUTE_NAME: &str = "world_query_data";

pub fn derive_world_query_data_impl(input: TokenStream) -> TokenStream {
    let tokens = input.clone();

    let ast = parse_macro_input!(input as DeriveInput);
    let visibility = ast.vis;

    let mut attributes = WorldQueryDataAttributes::default();
    for attr in &ast.attrs {
        if !attr
            .path()
            .get_ident()
            .map_or(false, |ident| ident == WORLD_QUERY_DATA_ATTRIBUTE_NAME)
        {
            continue;
        }

        attr.parse_args_with(|input: ParseStream| {
            let meta = input.parse_terminated(syn::Meta::parse, Comma)?;
            for meta in meta {
                let ident = meta.path().get_ident().unwrap_or_else(|| {
                    panic!(
                        "Unrecognized attribute: `{}`",
                        meta.path().to_token_stream()
                    )
                });
                if ident == MUTABLE_ATTRIBUTE_NAME {
                    if let syn::Meta::Path(_) = meta {
                        attributes.is_mutable = true;
                    } else {
                        panic!(
                            "The `{MUTABLE_ATTRIBUTE_NAME}` attribute is expected to have no value or arguments",
                        );
                    }
                }
                else if ident == DERIVE_ATTRIBUTE_NAME {
                    if let syn::Meta::List(meta_list) = meta {
                        meta_list.parse_nested_meta(|meta| {
                            attributes.derive_args.push(Meta::Path(meta.path));
                            Ok(())
                        })?;
                    } else {
                        panic!(
                            "Expected a structured list within the `{DERIVE_ATTRIBUTE_NAME}` attribute",
                        );
                    }
                } else {
                    panic!(
                        "Unrecognized attribute: `{}`",
                        meta.path().to_token_stream()
                    );
                }
            }
            Ok(())
        })
        .unwrap_or_else(|_| panic!("Invalid `{WORLD_QUERY_DATA_ATTRIBUTE_NAME}` attribute format"));
    }

    let Data::Struct(DataStruct { fields, .. }) = &ast.data else {
        return syn::Error::new(
            Span::call_site(),
            "#[derive(WorldQueryData)]` only supports structs",
        )
        .into_compile_error()
        .into();
    };

    // check field attributes
    for field in fields.iter() {
        for attr in &field.attrs {
            if attr
                .path()
                .get_ident()
                .is_some_and(|ident| ident == WORLD_QUERY_DATA_ATTRIBUTE_NAME)
            {
                return syn::Error::new_spanned(
                    attr,
                    "#[derive(WorldQueryData)] does not support field attributes.",
                )
                .into_compile_error()
                .into();
            }
        }
    }

    let path = bevy_ecs_path();

    let read_only_struct_name = if attributes.is_mutable {
        Ident::new(&format!("{}ReadOnly", ast.ident), Span::call_site())
    } else {
        ast.ident.clone()
    };

    let names = NamesInfo::new(ast.ident, &tokens);

    let fields_information = FieldsInfo::new(fields.clone());

    let derive_args = &attributes.derive_args;
    // `#[derive()]` is valid syntax
    let derive_macro_call = quote! { #[derive(#derive_args)] };

    let mutable_item_struct = item_struct(
        &path,
        &visibility,
        &fields_information,
        &names,
        &ast.generics,
        &derive_macro_call,
    );
    let mutable_world_query_impl = world_query_impl(
        &path,
        &visibility,
        &fields_information,
        &names,
        &ast.generics,
    );

    let NamesInfo {
        struct_name,
        state_struct_name,
        ..
    } = names.clone();

    let FieldsInfo {
        field_types,
        field_visibilities,
        field_idents,
        named_field_idents,
        ..
    } = fields_information.clone();

    let user_generics_with_world = world_query::add_world_lifetime(&ast.generics);
    let (user_impl_generics, user_ty_generics, user_where_clauses) = ast.generics.split_for_impl();
    let (user_impl_generics_with_world, _, user_where_clauses_with_world) =
        user_generics_with_world.split_for_impl();

    let read_only_struct: proc_macro2::TokenStream;
    let read_only_impl: proc_macro2::TokenStream;
    let read_only_data_impl: proc_macro2::TokenStream;
    let read_only_asserts: proc_macro2::TokenStream;

    if attributes.is_mutable {
        let fields_information_readonly = fields_information.clone().make_readonly(&path);

        let readonly_names = names.make_readonly(read_only_struct_name.clone(), &tokens);
        let readonly_item_struct = item_struct(
            &path,
            &visibility,
            &fields_information_readonly,
            &readonly_names,
            &ast.generics,
            &derive_macro_call,
        );
        read_only_impl = world_query_impl(
            &path,
            &visibility,
            &fields_information_readonly,
            &readonly_names,
            &ast.generics,
        );

        let read_only_field_types = fields_information_readonly.field_types;

        read_only_struct = quote! {
            #[doc = "Automatically generated [`WorldQuery`] type for a read-only variant of [`"]
            #[doc = stringify!(#struct_name)]
            #[doc = "`]."]
            #[automatically_derived]
            #visibility struct #read_only_struct_name #user_impl_generics #user_where_clauses {
                #(
                    #[doc = "Automatically generated read-only field for accessing `"]
                    #[doc = stringify!(#field_types)]
                    #[doc = "`."]
                    #field_visibilities #named_field_idents: #read_only_field_types,
                )*
            }

            #readonly_item_struct
        };

        read_only_data_impl = quote! {
            /// SAFETY: we assert fields are readonly below
            unsafe impl #user_impl_generics #path::query::WorldQueryData
            for #read_only_struct_name #user_ty_generics #user_where_clauses {
                type ReadOnly = #read_only_struct_name #user_ty_generics;
            }
        };

        read_only_asserts = quote! {
            // Double-check that the data fetched by `<_ as WorldQuery>::ReadOnly` is read-only.
            // This is technically unnecessary as `<_ as WorldQuery>::ReadOnly: ReadOnlyWorldQueryData`
            // but to protect against future mistakes we assert the assoc type implements `ReadOnlyWorldQueryData` anyway
            #( assert_readonly::<#read_only_field_types>(); )*
        };
    } else {
        read_only_struct = quote! {};
        read_only_impl = quote! {};
        read_only_data_impl = quote! {};
        read_only_asserts = quote! {
            // Statically checks that the safety guarantee of `ReadOnlyWorldQueryData` for `$fetch_struct_name` actually holds true.
            // We need this to make sure that we don't compile `ReadOnlyWorldQueryData` if our struct contains nested `WorldQueryData`
            // members that don't implement it. I.e.:
            // ```
            // #[derive(WorldQueryData)]
            // pub struct Foo { a: &'static mut MyComponent }
            // ```
            #( assert_readonly::<#field_types>(); )*
        };
    }

    let data_impl = quote! {
        /// SAFETY: we assert fields are readonly below
        unsafe impl #user_impl_generics #path::query::WorldQueryData
        for #struct_name #user_ty_generics #user_where_clauses {
            type ReadOnly = #read_only_struct_name #user_ty_generics;
        }

        #read_only_data_impl
    };

    let read_only_data_impl = quote! {
        /// SAFETY: we assert fields are readonly below
        unsafe impl #user_impl_generics #path::query::ReadOnlyWorldQueryData
        for #read_only_struct_name #user_ty_generics #user_where_clauses {}
    };

    let data_asserts = quote! {
        #( assert_data::<#field_types>(); )*
    };

    TokenStream::from(quote! {
        #mutable_item_struct

        #read_only_struct

        const _: () = {
            #[doc(hidden)]
            #[doc = "Automatically generated internal [`WorldQuery`] state type for [`"]
            #[doc = stringify!(#struct_name)]
            #[doc = "`], used for caching."]
            #[automatically_derived]
            #visibility struct #state_struct_name #user_impl_generics #user_where_clauses {
                #(#named_field_idents: <#field_types as #path::query::WorldQuery>::State,)*
            }

            #mutable_world_query_impl

            #read_only_impl

            #data_impl

            #read_only_data_impl
        };

        #[allow(dead_code)]
        const _: () = {
            fn assert_readonly<T>()
            where
                T: #path::query::ReadOnlyWorldQueryData,
            {
            }

            fn assert_data<T>()
            where
                T: #path::query::WorldQueryData,
            {
            }

            // We generate a readonly assertion for every struct member.
            fn assert_all #user_impl_generics_with_world () #user_where_clauses_with_world {
                #read_only_asserts
                #data_asserts
            }
        };

        // The original struct will most likely be left unused. As we don't want our users having
        // to specify `#[allow(dead_code)]` for their custom queries, we are using this cursed
        // workaround.
        #[allow(dead_code)]
        const _: () = {
            fn dead_code_workaround #user_impl_generics (
                q: #struct_name #user_ty_generics,
                q2: #read_only_struct_name #user_ty_generics
            ) #user_where_clauses {
                #(q.#field_idents;)*
                #(q2.#field_idents;)*
            }
        };
    })
}
