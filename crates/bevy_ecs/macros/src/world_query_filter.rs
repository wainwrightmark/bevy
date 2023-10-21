use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::{parse_macro_input, Data, DataStruct, DeriveInput};

use crate::{
    bevy_ecs_path,
    world_query::{self, item_struct, world_query_impl, FieldsInfo, NamesInfo},
};

mod field_attr_keywords {
    syn::custom_keyword!(ignore);
}

pub fn derive_world_query_filter_impl(input: TokenStream) -> TokenStream {
    let tokens = input.clone();

    let ast = parse_macro_input!(input as DeriveInput);
    let visibility = ast.vis;

    let path = bevy_ecs_path();

    let names = NamesInfo::new(ast.ident, &tokens);

    let Data::Struct(DataStruct { fields, .. }) = &ast.data else {
        return syn::Error::new(
            Span::call_site(),
            "#[derive(WorldQuery)]` only supports structs",
        )
        .into_compile_error()
        .into();
    };

    let fields_information = FieldsInfo::new(fields.clone());

    let derive_macro_call = quote!();

    let item_struct = item_struct(
        &path,
        &visibility,
        &fields_information,
        &names,
        &ast.generics,
        &derive_macro_call,
    );

    let world_query_impl = world_query_impl(
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
    } = names;

    let FieldsInfo {
        field_types,
        field_idents,
        named_field_idents,
        ..
    } = fields_information;

    let user_generics_with_world = world_query::add_world_lifetime(&ast.generics);
    let (user_impl_generics, user_ty_generics, user_where_clauses) = ast.generics.split_for_impl();
    let (user_impl_generics_with_world, _, user_where_clauses_with_world) =
        user_generics_with_world.split_for_impl();

    let filter_impl = quote! {
        impl #user_impl_generics #path::query::WorldQueryFilter
        for #struct_name #user_ty_generics #user_where_clauses {
            const IS_ARCHETYPAL: bool = true #(&& <#field_types>::IS_ARCHETYPAL)*;

            #[allow(unused_variables)]
            #[inline(always)]
            unsafe fn filter_fetch<'__w>(
                _fetch: &mut <Self as #path::query::WorldQuery>::Fetch<'__w>,
                _entity: #path::entity::Entity,
                _table_row: #path::storage::TableRow,
            ) -> bool {
                true #(&& <#field_types>::filter_fetch(&mut _fetch.#named_field_idents, _entity, _table_row))*
            }
        }
    };

    let filter_asserts = quote! {
        #( assert_filter::<#field_types>(); )*
    };

    TokenStream::from(quote! {
        #item_struct

        const _: () = {
            #[doc(hidden)]
            #[doc = "Automatically generated internal [`WorldQuery`] state type for [`"]
            #[doc = stringify!(#struct_name)]
            #[doc = "`], used for caching."]
            #[automatically_derived]
            #visibility struct #state_struct_name #user_impl_generics #user_where_clauses {
                #(#named_field_idents: <#field_types as #path::query::WorldQuery>::State,)*
            }

            #world_query_impl

            #filter_impl
        };

        #[allow(dead_code)]
        const _: () = {

            fn assert_filter<T>()
            where
                T: #path::query::WorldQueryFilter,
            {
            }

            // We generate a filter assertion for every struct member.
            fn assert_all #user_impl_generics_with_world () #user_where_clauses_with_world {
                #filter_asserts
            }
        };

        // The original struct will most likely be left unused. As we don't want our users having
        // to specify `#[allow(dead_code)]` for their custom queries, we are using this cursed
        // workaround.
        #[allow(dead_code)]
        const _: () = {
            fn dead_code_workaround #user_impl_generics (
                q: #struct_name #user_ty_generics,
                q2: #struct_name #user_ty_generics
            ) #user_where_clauses {
                #(q.#field_idents;)*
                #(q2.#field_idents;)*
            }
        };
    })
}
