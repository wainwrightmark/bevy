use bevy_macro_utils::ensure_no_collision;
use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::{format_ident, quote};
use syn::{parse_quote, Index};
use syn::{Attribute, Fields, Generics, Visibility};

#[derive(Clone)]
pub(crate) struct NamesInfo {
    pub struct_name: Ident,
    pub item_struct_name: Ident,
    pub fetch_struct_name: Ident,
    pub marker_name: Ident,
    pub state_struct_name: Ident,
}

impl NamesInfo {
    pub(crate) fn new(struct_name: Ident, tokens: &TokenStream) -> Self {
        let item_struct_name = Ident::new(&format!("{struct_name}Item"), Span::call_site());

        let fetch_struct_name = Ident::new(&format!("{struct_name}Fetch"), Span::call_site());
        let fetch_struct_name = ensure_no_collision(fetch_struct_name, tokens.clone());

        let marker_name =
            ensure_no_collision(format_ident!("_world_query_derive_marker"), tokens.clone());

        // Generate a name for the state struct that doesn't conflict
        // with the struct definition.
        let state_struct_name = Ident::new(&format!("{struct_name}State"), Span::call_site());
        let state_struct_name = ensure_no_collision(state_struct_name, tokens.clone());

        Self {
            struct_name,
            item_struct_name,
            fetch_struct_name,
            marker_name,
            state_struct_name,
        }
    }

    pub fn make_readonly(self, readonly_struct_name: Ident, tokens: &TokenStream) -> Self {
        let item_struct_name =
            Ident::new(&format!("{readonly_struct_name}Item"), Span::call_site());
        let fetch_struct_name =
            Ident::new(&format!("{readonly_struct_name}Fetch"), Span::call_site());
        let fetch_struct_name = ensure_no_collision(fetch_struct_name, tokens.clone());

        Self {
            struct_name: readonly_struct_name,
            item_struct_name,
            fetch_struct_name,
            ..self
        }
    }
}

#[derive(Clone)]
pub(crate) struct FieldsInfo {
    pub fields: Fields,
    pub field_types: Vec<proc_macro2::TokenStream>,
    pub field_attrs: Vec<Vec<Attribute>>,
    pub field_visibilities: Vec<Visibility>,
    pub field_idents: Vec<proc_macro2::TokenStream>,
    pub named_field_idents: Vec<Ident>,
}

impl FieldsInfo {
    pub(crate) fn new(fields: Fields) -> Self {
        let mut field_attrs = Vec::new();
        let mut field_visibilities = Vec::new();
        let mut field_idents = Vec::new();
        let mut named_field_idents = Vec::new();
        let mut field_types = Vec::new();
        for (i, field) in fields.iter().enumerate() {
            let attrs = field.attrs.clone();

            let named_field_ident = field
                .ident
                .as_ref()
                .cloned()
                .unwrap_or_else(|| format_ident!("f{i}"));
            let i = Index::from(i);
            let field_ident = field
                .ident
                .as_ref()
                .map_or(quote! { #i }, |i| quote! { #i });
            field_idents.push(field_ident);
            named_field_idents.push(named_field_ident);
            field_attrs.push(attrs);
            field_visibilities.push(field.vis.clone());
            let field_ty = field.ty.clone();
            field_types.push(quote!(#field_ty));
        }

        Self {
            fields,
            field_types,
            field_attrs,
            field_visibilities,
            field_idents,
            named_field_idents,
        }
    }

    pub fn make_readonly(mut self, bevy_ecs_path: &syn::Path) -> Self {
        self.field_types = self
            .fields
            .iter()
            .map(|field| {
                let field_ty = field.ty.clone();
                quote!(<#field_ty as #bevy_ecs_path::query::WorldQueryData>::ReadOnly)
            })
            .collect();

        self
    }
}

pub(crate) fn add_world_lifetime(generics: &Generics) -> Generics {
    let mut generics = generics.clone();
    generics.params.insert(0, parse_quote!('__w));
    generics
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn item_struct(
    path: &syn::Path,
    visibility: &Visibility,
    fields_information: &FieldsInfo,
    names: &NamesInfo,
    user_generics: &Generics,
    derive_macro_call: &proc_macro2::TokenStream,
) -> proc_macro2::TokenStream {
    let NamesInfo {
        struct_name,
        item_struct_name,
        ..
    } = names;

    let user_generics_with_world = add_world_lifetime(user_generics);
    let (_, user_ty_generics, _) = user_generics.split_for_impl();
    let (user_impl_generics_with_world, user_ty_generics_with_world, user_where_clauses_with_world) =
        user_generics_with_world.split_for_impl();

    let FieldsInfo {
        fields,
        field_types,
        field_attrs,
        field_visibilities,
        field_idents,
        ..
    } = fields_information;

    let item_attrs = quote!(
            #[doc = "Automatically generated [`WorldQuery`] item type for [`"]
            #[doc = stringify!(#struct_name)]
            #[doc = "`], returned when iterating over query results."]
            #[automatically_derived]
    );

    match fields {
        syn::Fields::Named(_) => quote! {
            #derive_macro_call
            #item_attrs
            #visibility struct #item_struct_name #user_impl_generics_with_world #user_where_clauses_with_world {
                #(#(#field_attrs)* #field_visibilities #field_idents: <#field_types as #path::query::WorldQuery>::Item<'__w>,)*
            }
        },
        syn::Fields::Unnamed(_) => quote! {
            #derive_macro_call
            #item_attrs
            #[automatically_derived]
            #visibility struct #item_struct_name #user_impl_generics_with_world #user_where_clauses_with_world(
                #( #field_visibilities <#field_types as #path::query::WorldQuery>::Item<'__w>, )*
            );
        },
        syn::Fields::Unit => quote! {
            #item_attrs
            #visibility type #item_struct_name #user_ty_generics_with_world = #struct_name #user_ty_generics;
        },
    }
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn world_query_impl(
    path: &syn::Path,
    visibility: &Visibility,
    fields_information: &FieldsInfo,
    names: &NamesInfo,
    user_generics: &Generics,
) -> proc_macro2::TokenStream {
    let NamesInfo {
        struct_name,
        item_struct_name,
        fetch_struct_name,
        marker_name,
        state_struct_name,
    } = names;

    let user_generics_with_world = add_world_lifetime(user_generics);
    let (user_impl_generics, user_ty_generics, user_where_clauses) = user_generics.split_for_impl();
    let (user_impl_generics_with_world, user_ty_generics_with_world, user_where_clauses_with_world) =
        user_generics_with_world.split_for_impl();

    let FieldsInfo {
        field_types,
        field_idents,
        named_field_idents,
        ..
    } = fields_information;

    quote! {
        #[doc(hidden)]
        #[doc = "Automatically generated internal [`WorldQuery`] fetch type for [`"]
        #[doc = stringify!(#struct_name)]
        #[doc = "`], used to define the world data accessed by this query."]
        #[automatically_derived]
        #visibility struct #fetch_struct_name #user_impl_generics_with_world #user_where_clauses_with_world {
            #(#named_field_idents: <#field_types as #path::query::WorldQuery>::Fetch<'__w>,)*
            #marker_name: &'__w (),
        }

        impl #user_impl_generics_with_world Clone for #fetch_struct_name #user_ty_generics_with_world
            #user_where_clauses_with_world {
                fn clone(&self) -> Self {
                    Self {
                        #(#named_field_idents: self.#named_field_idents.clone(),)*
                        #marker_name: &(),
                    }
                }
            }

        // SAFETY: `update_component_access` and `update_archetype_component_access` are called on every field
        unsafe impl #user_impl_generics #path::query::WorldQuery
            for #struct_name #user_ty_generics #user_where_clauses {

            type Item<'__w> = #item_struct_name #user_ty_generics_with_world;
            type Fetch<'__w> = #fetch_struct_name #user_ty_generics_with_world;
            type State = #state_struct_name #user_ty_generics;

            fn shrink<'__wlong: '__wshort, '__wshort>(
                item: <#struct_name #user_ty_generics as #path::query::WorldQuery>::Item<'__wlong>
            ) -> <#struct_name #user_ty_generics as #path::query::WorldQuery>::Item<'__wshort> {
                #item_struct_name {
                    #(
                        #field_idents: <#field_types>::shrink(item.#field_idents),
                    )*
                }
            }

            unsafe fn init_fetch<'__w>(
                _world: #path::world::unsafe_world_cell::UnsafeWorldCell<'__w>,
                state: &Self::State,
                _last_run: #path::component::Tick,
                _this_run: #path::component::Tick,
            ) -> <Self as #path::query::WorldQuery>::Fetch<'__w> {
                #fetch_struct_name {
                    #(#named_field_idents:
                        <#field_types>::init_fetch(
                            _world,
                            &state.#named_field_idents,
                            _last_run,
                            _this_run,
                        ),
                    )*
                    #marker_name: &(),
                }
            }

            const IS_DENSE: bool = true #(&& <#field_types>::IS_DENSE)*;

            /// SAFETY: we call `set_archetype` for each member that implements `Fetch`
            #[inline]
            unsafe fn set_archetype<'__w>(
                _fetch: &mut <Self as #path::query::WorldQuery>::Fetch<'__w>,
                _state: &Self::State,
                _archetype: &'__w #path::archetype::Archetype,
                _table: &'__w #path::storage::Table
            ) {
                #(<#field_types>::set_archetype(&mut _fetch.#named_field_idents, &_state.#named_field_idents, _archetype, _table);)*
            }

            /// SAFETY: we call `set_table` for each member that implements `Fetch`
            #[inline]
            unsafe fn set_table<'__w>(
                _fetch: &mut <Self as #path::query::WorldQuery>::Fetch<'__w>,
                _state: &Self::State,
                _table: &'__w #path::storage::Table
            ) {
                #(<#field_types>::set_table(&mut _fetch.#named_field_idents, &_state.#named_field_idents, _table);)*
            }

            /// SAFETY: we call `fetch` for each member that implements `Fetch`.
            #[inline(always)]
            unsafe fn fetch<'__w>(
                _fetch: &mut <Self as #path::query::WorldQuery>::Fetch<'__w>,
                _entity: #path::entity::Entity,
                _table_row: #path::storage::TableRow,
            ) -> <Self as #path::query::WorldQuery>::Item<'__w> {
                Self::Item {
                    #(#field_idents: <#field_types>::fetch(&mut _fetch.#named_field_idents, _entity, _table_row),)*
                }
            }

            fn update_component_access(state: &Self::State, _access: &mut #path::query::FilteredAccess<#path::component::ComponentId>) {
                #( <#field_types>::update_component_access(&state.#named_field_idents, _access); )*
            }

            fn update_archetype_component_access(
                state: &Self::State,
                _archetype: &#path::archetype::Archetype,
                _access: &mut #path::query::Access<#path::archetype::ArchetypeComponentId>
            ) {
                #(
                    <#field_types>::update_archetype_component_access(&state.#named_field_idents, _archetype, _access);
                )*
            }

            fn init_state(world: &mut #path::world::World) -> #state_struct_name #user_ty_generics {
                #state_struct_name {
                    #(#named_field_idents: <#field_types>::init_state(world),)*
                }
            }

            fn matches_component_set(state: &Self::State, _set_contains_id: &impl Fn(#path::component::ComponentId) -> bool) -> bool {
                true #(&& <#field_types>::matches_component_set(&state.#named_field_idents, _set_contains_id))*
            }
        }
    }
}
