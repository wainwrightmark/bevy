use crate::{
    archetype::{Archetype, ArchetypeComponentId},
    change_detection::{Ticks, TicksMut},
    component::{Component, ComponentId, ComponentStorage, StorageType, Tick},
    entity::Entity,
    query::{Access, DebugCheckedUnwrap, FilteredAccess, WorldQuery},
    storage::{ComponentSparseSet, Table, TableRow},
    world::{unsafe_world_cell::UnsafeWorldCell, Mut, Ref, World},
};
use bevy_ptr::{ThinSlicePtr, UnsafeCellDeref};
use bevy_utils::all_tuples;
use std::{cell::UnsafeCell, marker::PhantomData};

/// Types that can be fetched from a [`World`] using a [`Query`].
///
/// There are many types that natively implement this trait:
///
/// - **Component references.**
///   Fetches a component by reference (immutably or mutably).
/// - **`WorldQueryData` tuples.**
///   If every element of a tuple implements `WorldQueryData`, then the tuple itself also implements the same trait.
///   This enables a single `Query` to access multiple components and filter over multiple conditions.
///   Due to the current lack of variadic generics in Rust, the trait has been implemented for tuples from 0 to 15 elements,
///   but nesting of tuples allows infinite `WorldQuery`s.
pub unsafe trait WorldQueryData: WorldQuery {
    /// The read-only variant of this [`WorldQueryData`], which satisfies the [`ReadOnlyWorldQueryData`] trait.
    type ReadOnly: ReadOnlyWorldQueryData<State = <Self as WorldQuery>::State>;

    /// This function manually implements subtyping for the query items.
    fn shrink<'wlong: 'wshort, 'wshort>(item: Self::Item<'wlong>) -> Self::Item<'wshort>;
}

/// A world query data that is read only.
///
/// # Safety
///
/// This must only be implemented for read-only [`WorldQuery`]'s.
pub unsafe trait ReadOnlyWorldQueryData: WorldQueryData<ReadOnly = Self> {}

/// The `Fetch` of a [`WorldQuery`], which is used to store state for each archetype/table.
pub type QueryFetch<'w, Q> = <Q as WorldQuery>::Fetch<'w>;
/// The item type returned when a [`WorldQuery`] is iterated over
pub type QueryItem<'w, Q> = <Q as WorldQuery>::Item<'w>;
/// The read-only `Fetch` of a [`WorldQuery`], which is used to store state for each archetype/table.
pub type ROQueryFetch<'w, Q> = QueryFetch<'w, <Q as WorldQueryData>::ReadOnly>;
/// The read-only variant of the item type returned when a [`WorldQuery`] is iterated over immutably
pub type ROQueryItem<'w, Q> = QueryItem<'w, <Q as WorldQueryData>::ReadOnly>;

/// SAFETY: no component or archetype access
unsafe impl WorldQuery for Entity {
    type Fetch<'w> = ();
    type Item<'w> = Entity;

    type State = ();

    const IS_DENSE: bool = true;

    unsafe fn init_fetch<'w>(
        _world: UnsafeWorldCell<'w>,
        _state: &Self::State,
        _last_run: Tick,
        _this_run: Tick,
    ) -> Self::Fetch<'w> {
    }

    unsafe fn clone_fetch<'w>(_fetch: &Self::Fetch<'w>) -> Self::Fetch<'w> {}

    #[inline]
    unsafe fn set_archetype<'w>(
        _fetch: &mut Self::Fetch<'w>,
        _state: &Self::State,
        _archetype: &'w Archetype,
        _table: &Table,
    ) {
    }

    #[inline]
    unsafe fn set_table<'w>(_fetch: &mut Self::Fetch<'w>, _state: &Self::State, _table: &'w Table) {
    }

    #[inline(always)]
    unsafe fn fetch<'w>(
        _fetch: &mut Self::Fetch<'w>,
        entity: Entity,
        _table_row: TableRow,
    ) -> Self::Item<'w> {
        entity
    }

    fn update_component_access(_state: &Self::State, _access: &mut FilteredAccess<ComponentId>) {}

    fn update_archetype_component_access(
        _state: &Self::State,
        _archetype: &Archetype,
        _access: &mut Access<ArchetypeComponentId>,
    ) {
    }

    fn init_state(_world: &mut World) {}

    fn matches_component_set(
        _state: &Self::State,
        _set_contains_id: &impl Fn(ComponentId) -> bool,
    ) -> bool {
        true
    }
}

unsafe impl WorldQueryData for Entity {
    type ReadOnly = Self;

    fn shrink<'wlong: 'wshort, 'wshort>(item: Self::Item<'wlong>) -> Self::Item<'wshort> {
        item
    }
}

/// SAFETY: access is read only
unsafe impl ReadOnlyWorldQueryData for Entity {}

#[doc(hidden)]
pub struct ReadFetch<'w, T> {
    // T::Storage = TableStorage
    table_components: Option<ThinSlicePtr<'w, UnsafeCell<T>>>,
    // T::Storage = SparseStorage
    sparse_set: Option<&'w ComponentSparseSet>,
}

/// SAFETY: `Self` is the same as `Self::ReadOnly`
unsafe impl<T: Component> WorldQuery for &T {
    type Fetch<'w> = ReadFetch<'w, T>;
    type Item<'w> = &'w T;
    type State = ComponentId;

    const IS_DENSE: bool = {
        match T::Storage::STORAGE_TYPE {
            StorageType::Table => true,
            StorageType::SparseSet => false,
        }
    };

    #[inline]
    unsafe fn init_fetch<'w>(
        world: UnsafeWorldCell<'w>,
        &component_id: &ComponentId,
        _last_run: Tick,
        _this_run: Tick,
    ) -> ReadFetch<'w, T> {
        ReadFetch {
            table_components: None,
            sparse_set: (T::Storage::STORAGE_TYPE == StorageType::SparseSet).then(|| {
                world
                    // SAFETY: The underlying type associated with `component_id` is `T`,
                    // which we are allowed to access since we registered it in `update_archetype_component_access`.
                    // Note that we do not actually access any components in this function, we just get a shared
                    // reference to the sparse set, which is used to access the components in `Self::fetch`.
                    .unsafe_world()
                    .storages()
                    .sparse_sets
                    .get(component_id)
                    .debug_checked_unwrap()
            }),
        }
    }

    unsafe fn clone_fetch<'w>(fetch: &Self::Fetch<'w>) -> Self::Fetch<'w> {
        ReadFetch {
            table_components: fetch.table_components,
            sparse_set: fetch.sparse_set,
        }
    }

    #[inline]
    unsafe fn set_archetype<'w>(
        fetch: &mut ReadFetch<'w, T>,
        component_id: &ComponentId,
        _archetype: &'w Archetype,
        table: &'w Table,
    ) {
        if Self::IS_DENSE {
            Self::set_table(fetch, component_id, table);
        }
    }

    #[inline]
    unsafe fn set_table<'w>(
        fetch: &mut ReadFetch<'w, T>,
        &component_id: &ComponentId,
        table: &'w Table,
    ) {
        fetch.table_components = Some(
            table
                .get_column(component_id)
                .debug_checked_unwrap()
                .get_data_slice()
                .into(),
        );
    }

    #[inline(always)]
    unsafe fn fetch<'w>(
        fetch: &mut Self::Fetch<'w>,
        entity: Entity,
        table_row: TableRow,
    ) -> Self::Item<'w> {
        match T::Storage::STORAGE_TYPE {
            StorageType::Table => fetch
                .table_components
                .debug_checked_unwrap()
                .get(table_row.index())
                .deref(),
            StorageType::SparseSet => fetch
                .sparse_set
                .debug_checked_unwrap()
                .get(entity)
                .debug_checked_unwrap()
                .deref(),
        }
    }

    fn update_component_access(
        &component_id: &ComponentId,
        access: &mut FilteredAccess<ComponentId>,
    ) {
        assert!(
            !access.access().has_write(component_id),
            "&{} conflicts with a previous access in this query. Shared access cannot coincide with exclusive access.",
                std::any::type_name::<T>(),
        );
        access.add_read(component_id);
    }

    fn update_archetype_component_access(
        &component_id: &ComponentId,
        archetype: &Archetype,
        access: &mut Access<ArchetypeComponentId>,
    ) {
        if let Some(archetype_component_id) = archetype.get_archetype_component_id(component_id) {
            access.add_read(archetype_component_id);
        }
    }

    fn init_state(world: &mut World) -> ComponentId {
        world.init_component::<T>()
    }

    fn matches_component_set(
        &state: &ComponentId,
        set_contains_id: &impl Fn(ComponentId) -> bool,
    ) -> bool {
        set_contains_id(state)
    }
}

unsafe impl<T: Component> WorldQueryData for &T {
    type ReadOnly = Self;

    fn shrink<'wlong: 'wshort, 'wshort>(item: &'wlong T) -> &'wshort T {
        item
    }
}

/// SAFETY: access is read only
unsafe impl<T: Component> ReadOnlyWorldQueryData for &T {}

#[doc(hidden)]
pub struct RefFetch<'w, T> {
    // T::Storage = TableStorage
    table_data: Option<(
        ThinSlicePtr<'w, UnsafeCell<T>>,
        ThinSlicePtr<'w, UnsafeCell<Tick>>,
        ThinSlicePtr<'w, UnsafeCell<Tick>>,
    )>,
    // T::Storage = SparseStorage
    sparse_set: Option<&'w ComponentSparseSet>,

    last_run: Tick,
    this_run: Tick,
}

/// SAFETY: `Self` is the same as `Self::ReadOnly`
unsafe impl<'__w, T: Component> WorldQuery for Ref<'__w, T> {
    type Fetch<'w> = RefFetch<'w, T>;
    type Item<'w> = Ref<'w, T>;

    type State = ComponentId;

    const IS_DENSE: bool = {
        match T::Storage::STORAGE_TYPE {
            StorageType::Table => true,
            StorageType::SparseSet => false,
        }
    };

    #[inline]
    unsafe fn init_fetch<'w>(
        world: UnsafeWorldCell<'w>,
        &component_id: &ComponentId,
        last_run: Tick,
        this_run: Tick,
    ) -> RefFetch<'w, T> {
        RefFetch {
            table_data: None,
            sparse_set: (T::Storage::STORAGE_TYPE == StorageType::SparseSet).then(|| {
                world
                    // SAFETY: See &T::init_fetch.
                    .unsafe_world()
                    .storages()
                    .sparse_sets
                    .get(component_id)
                    .debug_checked_unwrap()
            }),
            last_run,
            this_run,
        }
    }

    unsafe fn clone_fetch<'w>(fetch: &Self::Fetch<'w>) -> Self::Fetch<'w> {
        RefFetch {
            table_data: fetch.table_data,
            sparse_set: fetch.sparse_set,
            last_run: fetch.last_run,
            this_run: fetch.this_run,
        }
    }

    #[inline]
    unsafe fn set_archetype<'w>(
        fetch: &mut RefFetch<'w, T>,
        component_id: &ComponentId,
        _archetype: &'w Archetype,
        table: &'w Table,
    ) {
        if Self::IS_DENSE {
            Self::set_table(fetch, component_id, table);
        }
    }

    #[inline]
    unsafe fn set_table<'w>(
        fetch: &mut RefFetch<'w, T>,
        &component_id: &ComponentId,
        table: &'w Table,
    ) {
        let column = table.get_column(component_id).debug_checked_unwrap();
        fetch.table_data = Some((
            column.get_data_slice().into(),
            column.get_added_ticks_slice().into(),
            column.get_changed_ticks_slice().into(),
        ));
    }

    #[inline(always)]
    unsafe fn fetch<'w>(
        fetch: &mut Self::Fetch<'w>,
        entity: Entity,
        table_row: TableRow,
    ) -> Self::Item<'w> {
        match T::Storage::STORAGE_TYPE {
            StorageType::Table => {
                let (table_components, added_ticks, changed_ticks) =
                    fetch.table_data.debug_checked_unwrap();
                Ref {
                    value: table_components.get(table_row.index()).deref(),
                    ticks: Ticks {
                        added: added_ticks.get(table_row.index()).deref(),
                        changed: changed_ticks.get(table_row.index()).deref(),
                        this_run: fetch.this_run,
                        last_run: fetch.last_run,
                    },
                }
            }
            StorageType::SparseSet => {
                let (component, ticks) = fetch
                    .sparse_set
                    .debug_checked_unwrap()
                    .get_with_ticks(entity)
                    .debug_checked_unwrap();
                Ref {
                    value: component.deref(),
                    ticks: Ticks::from_tick_cells(ticks, fetch.last_run, fetch.this_run),
                }
            }
        }
    }

    fn update_component_access(
        &component_id: &ComponentId,
        access: &mut FilteredAccess<ComponentId>,
    ) {
        assert!(
            !access.access().has_write(component_id),
            "&{} conflicts with a previous access in this query. Shared access cannot coincide with exclusive access.",
                std::any::type_name::<T>(),
        );
        access.add_read(component_id);
    }

    fn update_archetype_component_access(
        &component_id: &ComponentId,
        archetype: &Archetype,
        access: &mut Access<ArchetypeComponentId>,
    ) {
        if let Some(archetype_component_id) = archetype.get_archetype_component_id(component_id) {
            access.add_read(archetype_component_id);
        }
    }

    fn init_state(world: &mut World) -> ComponentId {
        world.init_component::<T>()
    }

    fn matches_component_set(
        &state: &ComponentId,
        set_contains_id: &impl Fn(ComponentId) -> bool,
    ) -> bool {
        set_contains_id(state)
    }
}

/// SAFETY: access is read only
unsafe impl<'__w, T: Component> WorldQueryData for Ref<'__w, T> {
    type ReadOnly = Self;

    fn shrink<'wlong: 'wshort, 'wshort>(item: Ref<'wlong, T>) -> Ref<'wshort, T> {
        item
    }
}

unsafe impl<'__w, T: Component> ReadOnlyWorldQueryData for Ref<'__w, T> {}

#[doc(hidden)]
pub struct WriteFetch<'w, T> {
    // T::Storage = TableStorage
    table_data: Option<(
        ThinSlicePtr<'w, UnsafeCell<T>>,
        ThinSlicePtr<'w, UnsafeCell<Tick>>,
        ThinSlicePtr<'w, UnsafeCell<Tick>>,
    )>,
    // T::Storage = SparseStorage
    sparse_set: Option<&'w ComponentSparseSet>,

    last_run: Tick,
    this_run: Tick,
}

/// SAFETY: access of `&T` is a subset of `&mut T`
unsafe impl<'__w, T: Component> WorldQuery for &'__w mut T {
    type Fetch<'w> = WriteFetch<'w, T>;
    type Item<'w> = Mut<'w, T>;
    type State = ComponentId;

    const IS_DENSE: bool = {
        match T::Storage::STORAGE_TYPE {
            StorageType::Table => true,
            StorageType::SparseSet => false,
        }
    };

    #[inline]
    unsafe fn init_fetch<'w>(
        world: UnsafeWorldCell<'w>,
        &component_id: &ComponentId,
        last_run: Tick,
        this_run: Tick,
    ) -> WriteFetch<'w, T> {
        WriteFetch {
            table_data: None,
            sparse_set: (T::Storage::STORAGE_TYPE == StorageType::SparseSet).then(|| {
                world
                    // SAFETY: See &T::init_fetch.
                    .unsafe_world()
                    .storages()
                    .sparse_sets
                    .get(component_id)
                    .debug_checked_unwrap()
            }),
            last_run,
            this_run,
        }
    }

    unsafe fn clone_fetch<'w>(fetch: &Self::Fetch<'w>) -> Self::Fetch<'w> {
        WriteFetch {
            table_data: fetch.table_data,
            sparse_set: fetch.sparse_set,
            last_run: fetch.last_run,
            this_run: fetch.this_run,
        }
    }

    #[inline]
    unsafe fn set_archetype<'w>(
        fetch: &mut WriteFetch<'w, T>,
        component_id: &ComponentId,
        _archetype: &'w Archetype,
        table: &'w Table,
    ) {
        if Self::IS_DENSE {
            Self::set_table(fetch, component_id, table);
        }
    }

    #[inline]
    unsafe fn set_table<'w>(
        fetch: &mut WriteFetch<'w, T>,
        &component_id: &ComponentId,
        table: &'w Table,
    ) {
        let column = table.get_column(component_id).debug_checked_unwrap();
        fetch.table_data = Some((
            column.get_data_slice().into(),
            column.get_added_ticks_slice().into(),
            column.get_changed_ticks_slice().into(),
        ));
    }

    #[inline(always)]
    unsafe fn fetch<'w>(
        fetch: &mut Self::Fetch<'w>,
        entity: Entity,
        table_row: TableRow,
    ) -> Self::Item<'w> {
        match T::Storage::STORAGE_TYPE {
            StorageType::Table => {
                let (table_components, added_ticks, changed_ticks) =
                    fetch.table_data.debug_checked_unwrap();
                Mut {
                    value: table_components.get(table_row.index()).deref_mut(),
                    ticks: TicksMut {
                        added: added_ticks.get(table_row.index()).deref_mut(),
                        changed: changed_ticks.get(table_row.index()).deref_mut(),
                        this_run: fetch.this_run,
                        last_run: fetch.last_run,
                    },
                }
            }
            StorageType::SparseSet => {
                let (component, ticks) = fetch
                    .sparse_set
                    .debug_checked_unwrap()
                    .get_with_ticks(entity)
                    .debug_checked_unwrap();
                Mut {
                    value: component.assert_unique().deref_mut(),
                    ticks: TicksMut::from_tick_cells(ticks, fetch.last_run, fetch.this_run),
                }
            }
        }
    }

    fn update_component_access(
        &component_id: &ComponentId,
        access: &mut FilteredAccess<ComponentId>,
    ) {
        assert!(
            !access.access().has_read(component_id),
            "&mut {} conflicts with a previous access in this query. Mutable component access must be unique.",
                std::any::type_name::<T>(),
        );
        access.add_write(component_id);
    }

    fn update_archetype_component_access(
        &component_id: &ComponentId,
        archetype: &Archetype,
        access: &mut Access<ArchetypeComponentId>,
    ) {
        if let Some(archetype_component_id) = archetype.get_archetype_component_id(component_id) {
            access.add_write(archetype_component_id);
        }
    }

    fn init_state(world: &mut World) -> ComponentId {
        world.init_component::<T>()
    }

    fn matches_component_set(
        &state: &ComponentId,
        set_contains_id: &impl Fn(ComponentId) -> bool,
    ) -> bool {
        set_contains_id(state)
    }
}

unsafe impl<'__w, T: Component> WorldQueryData for &'__w mut T {
    type ReadOnly = &'__w T;

    fn shrink<'wlong: 'wshort, 'wshort>(item: Mut<'wlong, T>) -> Mut<'wshort, T> {
        item
    }
}

#[doc(hidden)]
pub struct OptionFetch<'w, T: WorldQuery> {
    fetch: T::Fetch<'w>,
    matches: bool,
}

// SAFETY: defers to soundness of `T: WorldQuery` impl
unsafe impl<T: WorldQuery> WorldQuery for Option<T> {
    type Fetch<'w> = OptionFetch<'w, T>;
    type Item<'w> = Option<T::Item<'w>>;
    type State = T::State;

    const IS_DENSE: bool = T::IS_DENSE;

    #[inline]
    unsafe fn init_fetch<'w>(
        world: UnsafeWorldCell<'w>,
        state: &T::State,
        last_run: Tick,
        this_run: Tick,
    ) -> OptionFetch<'w, T> {
        OptionFetch {
            fetch: T::init_fetch(world, state, last_run, this_run),
            matches: false,
        }
    }

    unsafe fn clone_fetch<'w>(fetch: &Self::Fetch<'w>) -> Self::Fetch<'w> {
        OptionFetch {
            fetch: T::clone_fetch(&fetch.fetch),
            matches: fetch.matches,
        }
    }

    #[inline]
    unsafe fn set_archetype<'w>(
        fetch: &mut OptionFetch<'w, T>,
        state: &T::State,
        archetype: &'w Archetype,
        table: &'w Table,
    ) {
        fetch.matches = T::matches_component_set(state, &|id| archetype.contains(id));
        if fetch.matches {
            T::set_archetype(&mut fetch.fetch, state, archetype, table);
        }
    }

    #[inline]
    unsafe fn set_table<'w>(fetch: &mut OptionFetch<'w, T>, state: &T::State, table: &'w Table) {
        fetch.matches = T::matches_component_set(state, &|id| table.has_column(id));
        if fetch.matches {
            T::set_table(&mut fetch.fetch, state, table);
        }
    }

    #[inline(always)]
    unsafe fn fetch<'w>(
        fetch: &mut Self::Fetch<'w>,
        entity: Entity,
        table_row: TableRow,
    ) -> Self::Item<'w> {
        fetch
            .matches
            .then(|| T::fetch(&mut fetch.fetch, entity, table_row))
    }

    fn update_component_access(state: &T::State, access: &mut FilteredAccess<ComponentId>) {
        // We don't want to add the `with`/`without` of `T` as `Option<T>` will match things regardless of
        // `T`'s filters. for example `Query<(Option<&U>, &mut V)>` will match every entity with a `V` component
        // regardless of whether it has a `U` component. If we don't do this the query will not conflict with
        // `Query<&mut V, Without<U>>` which would be unsound.
        let mut intermediate = access.clone();
        T::update_component_access(state, &mut intermediate);
        access.extend_access(&intermediate);
    }

    fn update_archetype_component_access(
        state: &T::State,
        archetype: &Archetype,
        access: &mut Access<ArchetypeComponentId>,
    ) {
        if T::matches_component_set(state, &|id| archetype.contains(id)) {
            T::update_archetype_component_access(state, archetype, access);
        }
    }

    fn init_state(world: &mut World) -> T::State {
        T::init_state(world)
    }

    fn matches_component_set(
        _state: &T::State,
        _set_contains_id: &impl Fn(ComponentId) -> bool,
    ) -> bool {
        true
    }
}

// SAFETY: defers to soundness of `T: WorldQuery` impl
unsafe impl<T: WorldQueryData> WorldQueryData for Option<T> {
    type ReadOnly = Option<T::ReadOnly>;

    fn shrink<'wlong: 'wshort, 'wshort>(item: Self::Item<'wlong>) -> Self::Item<'wshort> {
        item.map(T::shrink)
    }
}

/// SAFETY: [`OptionFetch`] is read only because `T` is read only
unsafe impl<T: ReadOnlyWorldQueryData> ReadOnlyWorldQueryData for Option<T> {}

/// Returns a bool that describes if the entity has component `T`.
///
/// This can be used in a [`Query`](crate::system::Query) if you want to know whether or not entities
/// have the component `T`  but don't actually care about the component's value.
///
/// # Examples
///
/// ```
/// # use bevy_ecs::component::Component;
/// # use bevy_ecs::query::Has;
/// # use bevy_ecs::system::IntoSystem;
/// # use bevy_ecs::system::Query;
/// #
/// # #[derive(Component)]
/// # struct IsHungry;
/// # #[derive(Component)]
/// # struct Name { name: &'static str };
/// #
/// fn food_entity_system(query: Query<(&Name, Has<IsHungry>) >) {
///     for (name, is_hungry) in &query {
///         if is_hungry{
///             println!("{} would like some food.", name.name);
///         } else {
///             println!("{} has had sufficient.", name.name);
///         }
///     }
/// }
/// # bevy_ecs::system::assert_is_system(food_entity_system);
/// ```
///
/// ```
/// # use bevy_ecs::component::Component;
/// # use bevy_ecs::query::Has;
/// # use bevy_ecs::system::IntoSystem;
/// # use bevy_ecs::system::Query;
/// #
/// # #[derive(Component)]
/// # struct Alpha{has_beta: bool};
/// # #[derive(Component)]
/// # struct Beta { has_alpha: bool };
/// #
/// // Unlike `Option<&T>`, `Has<T>` is compatible with `&mut T`
/// // as it does not actually access any data.
/// fn alphabet_entity_system(mut alphas: Query<(&mut Alpha, Has<Beta>)>, mut betas: Query<(&mut Beta, Has<Alpha>)>) {
///     for (mut alpha, has_beta) in alphas.iter_mut() {
///         alpha.has_beta = has_beta;
///     }
///     for (mut beta, has_alpha) in betas.iter_mut() {
///         beta.has_alpha = has_alpha;
///     }
/// }
/// # bevy_ecs::system::assert_is_system(alphabet_entity_system);
/// ```
pub struct Has<T>(PhantomData<T>);

// SAFETY: `Self::ReadOnly` is the same as `Self`
unsafe impl<T: Component> WorldQuery for Has<T> {
    type Fetch<'w> = bool;
    type Item<'w> = bool;
    type State = ComponentId;

    const IS_DENSE: bool = {
        match T::Storage::STORAGE_TYPE {
            StorageType::Table => true,
            StorageType::SparseSet => false,
        }
    };

    #[inline]
    unsafe fn init_fetch<'w>(
        _world: UnsafeWorldCell<'w>,
        _state: &Self::State,
        _last_run: Tick,
        _this_run: Tick,
    ) -> Self::Fetch<'w> {
        false
    }

    unsafe fn clone_fetch<'w>(fetch: &Self::Fetch<'w>) -> Self::Fetch<'w> {
        *fetch
    }

    #[inline]
    unsafe fn set_archetype<'w>(
        fetch: &mut Self::Fetch<'w>,
        state: &Self::State,
        archetype: &'w Archetype,
        _table: &Table,
    ) {
        *fetch = archetype.contains(*state);
    }

    #[inline]
    unsafe fn set_table<'w>(fetch: &mut Self::Fetch<'w>, state: &Self::State, table: &'w Table) {
        *fetch = table.has_column(*state);
    }

    #[inline(always)]
    unsafe fn fetch<'w>(
        fetch: &mut Self::Fetch<'w>,
        _entity: Entity,
        _table_row: TableRow,
    ) -> Self::Item<'w> {
        *fetch
    }

    fn update_component_access(_state: &Self::State, _access: &mut FilteredAccess<ComponentId>) {
        // Do nothing as presence of `Has<T>` never affects whether two queries are disjoint
    }

    fn update_archetype_component_access(
        _state: &Self::State,
        _archetype: &Archetype,
        _access: &mut Access<ArchetypeComponentId>,
    ) {
    }

    fn init_state(world: &mut World) -> ComponentId {
        world.init_component::<T>()
    }

    fn matches_component_set(
        _state: &Self::State,
        _set_contains_id: &impl Fn(ComponentId) -> bool,
    ) -> bool {
        // `Has<T>` always matches
        true
    }
}

/// SAFETY: [`Has`] is read only
unsafe impl<T: Component> WorldQueryData for Has<T> {
    type ReadOnly = Self;

    fn shrink<'wlong: 'wshort, 'wshort>(item: Self::Item<'wlong>) -> Self::Item<'wshort> {
        item
    }
}

/// SAFETY: [`Has`] is read only
unsafe impl<T: Component> ReadOnlyWorldQueryData for Has<T> {}

/// The `AnyOf` query parameter fetches entities with any of the component types included in T.
///
/// `Query<AnyOf<(&A, &B, &mut C)>>` is equivalent to `Query<(Option<&A>, Option<&B>, Option<&mut C>), Or<(With<A>, With<B>, With<C>)>>`.
/// Each of the components in `T` is returned as an `Option`, as with `Option<A>` queries.
/// Entities are guaranteed to have at least one of the components in `T`.
pub struct AnyOf<T>(PhantomData<T>);

macro_rules! impl_tuple_world_query_data {
    ($(($name: ident, $state: ident)),*) => {

        #[allow(non_snake_case)]
        #[allow(clippy::unused_unit)]
        // SAFETY: defers to soundness `$name: WorldQuery` impl
        unsafe impl<$($name: WorldQueryData),*> WorldQueryData for ($($name,)*) {
            type ReadOnly = ($($name::ReadOnly,)*);

            fn shrink<'wlong: 'wshort, 'wshort>(item: Self::Item<'wlong>) -> Self::Item<'wshort> {
                let ($($name,)*) = item;
                ($(
                    $name::shrink($name),
                )*)
            }
        }

        /// SAFETY: each item in the tuple is read only
        unsafe impl<$($name: ReadOnlyWorldQueryData),*> ReadOnlyWorldQueryData for ($($name,)*) {}

    };
}

macro_rules! impl_anytuple_fetch {
    ($(($name: ident, $state: ident)),*) => {

        #[allow(non_snake_case)]
        #[allow(clippy::unused_unit)]
        // SAFETY: defers to soundness of `$name: WorldQuery` impl
        unsafe impl<$($name: WorldQuery),*> WorldQuery for AnyOf<($($name,)*)> {
            type Fetch<'w> = ($(($name::Fetch<'w>, bool),)*);
            type Item<'w> = ($(Option<$name::Item<'w>>,)*);
            type State = ($($name::State,)*);

            #[inline]
            #[allow(clippy::unused_unit)]
            unsafe fn init_fetch<'w>(_world: UnsafeWorldCell<'w>, state: &Self::State, _last_run: Tick, _this_run: Tick) -> Self::Fetch<'w> {
                let ($($name,)*) = state;
                ($(($name::init_fetch(_world, $name, _last_run, _this_run), false),)*)
            }

            unsafe fn clone_fetch<'w>(
                fetch: &Self::Fetch<'w>,
            ) -> Self::Fetch<'w> {
                let ($($name,)*) = &fetch;
                ($(($name::clone_fetch(& $name.0), $name.1),)*)
            }

            const IS_DENSE: bool = true $(&& $name::IS_DENSE)*;



            #[inline]
            unsafe fn set_archetype<'w>(
                _fetch: &mut Self::Fetch<'w>,
                _state: &Self::State,
                _archetype: &'w Archetype,
                _table: &'w Table
            ) {
                let ($($name,)*) = _fetch;
                let ($($state,)*) = _state;
                $(
                    $name.1 = $name::matches_component_set($state, &|id| _archetype.contains(id));
                    if $name.1 {
                        $name::set_archetype(&mut $name.0, $state, _archetype, _table);
                    }
                )*
            }

            #[inline]
            unsafe fn set_table<'w>(_fetch: &mut Self::Fetch<'w>, _state: &Self::State, _table: &'w Table) {
                let ($($name,)*) = _fetch;
                let ($($state,)*) = _state;
                $(
                    $name.1 = $name::matches_component_set($state, &|id| _table.has_column(id));
                    if $name.1 {
                        $name::set_table(&mut $name.0, $state, _table);
                    }
                )*
            }

            #[inline(always)]
            #[allow(clippy::unused_unit)]
            unsafe fn fetch<'w>(
                _fetch: &mut Self::Fetch<'w>,
                _entity: Entity,
                _table_row: TableRow
            ) -> Self::Item<'w> {
                let ($($name,)*) = _fetch;
                ($(
                    $name.1.then(|| $name::fetch(&mut $name.0, _entity, _table_row)),
                )*)
            }

            fn update_component_access(state: &Self::State, _access: &mut FilteredAccess<ComponentId>) {
                let ($($name,)*) = state;

                let mut _new_access = _access.clone();
                let mut _not_first = false;
                $(
                    if _not_first {
                        let mut intermediate = _access.clone();
                        $name::update_component_access($name, &mut intermediate);
                        _new_access.append_or(&intermediate);
                        _new_access.extend_access(&intermediate);
                    } else {
                        $name::update_component_access($name, &mut _new_access);
                        _not_first = true;
                    }
                )*

                *_access = _new_access;
            }

            fn update_archetype_component_access(state: &Self::State, _archetype: &Archetype, _access: &mut Access<ArchetypeComponentId>) {
                let ($($name,)*) = state;
                $(
                    if $name::matches_component_set($name, &|id| _archetype.contains(id)) {
                        $name::update_archetype_component_access($name, _archetype, _access);
                    }
                )*
            }

            fn init_state(_world: &mut World) -> Self::State {
                ($($name::init_state(_world),)*)
            }

            fn matches_component_set(_state: &Self::State, _set_contains_id: &impl Fn(ComponentId) -> bool) -> bool {
                let ($($name,)*) = _state;
                false $(|| $name::matches_component_set($name, _set_contains_id))*
            }
        }

        #[allow(non_snake_case)]
        #[allow(clippy::unused_unit)]
        // SAFETY: defers to soundness of `$name: WorldQuery` impl
        unsafe impl<$($name: WorldQueryData),*> WorldQueryData for AnyOf<($($name,)*)> {
            type ReadOnly = AnyOf<($($name::ReadOnly,)*)>;

            fn shrink<'wlong: 'wshort, 'wshort>(item: Self::Item<'wlong>) -> Self::Item<'wshort> {
                let ($($name,)*) = item;
                ($(
                    $name.map($name::shrink),
                )*)
            }
        }

        /// SAFETY: each item in the tuple is read only
        unsafe impl<$($name: ReadOnlyWorldQueryData),*> ReadOnlyWorldQueryData for AnyOf<($($name,)*)> {}
    };
}

all_tuples!(impl_tuple_world_query_data, 0, 15, F, S);
all_tuples!(impl_anytuple_fetch, 0, 15, F, S);

/// [`WorldQuery`] used to nullify queries by turning `Query<Q>` into `Query<NopWorldQuery<Q>>`
///
/// This will rarely be useful to consumers of `bevy_ecs`.
pub struct NopWorldQuery<Q: WorldQueryData>(PhantomData<Q>);

/// SAFETY: `Self::ReadOnly` is `Self`
unsafe impl<Q: WorldQueryData> WorldQuery for NopWorldQuery<Q> {
    type Fetch<'w> = ();
    type Item<'w> = ();
    type State = Q::State;

    const IS_DENSE: bool = Q::IS_DENSE;

    #[inline(always)]
    unsafe fn init_fetch(
        _world: UnsafeWorldCell,
        _state: &Q::State,
        _last_run: Tick,
        _this_run: Tick,
    ) {
    }

    unsafe fn clone_fetch<'w>(_fetch: &Self::Fetch<'w>) -> Self::Fetch<'w> {}

    #[inline(always)]
    unsafe fn set_archetype(
        _fetch: &mut (),
        _state: &Q::State,
        _archetype: &Archetype,
        _tables: &Table,
    ) {
    }

    #[inline(always)]
    unsafe fn set_table<'w>(_fetch: &mut (), _state: &Q::State, _table: &Table) {}

    #[inline(always)]
    unsafe fn fetch<'w>(
        _fetch: &mut Self::Fetch<'w>,
        _entity: Entity,
        _table_row: TableRow,
    ) -> Self::Item<'w> {
    }

    fn update_component_access(_state: &Q::State, _access: &mut FilteredAccess<ComponentId>) {}

    fn update_archetype_component_access(
        _state: &Q::State,
        _archetype: &Archetype,
        _access: &mut Access<ArchetypeComponentId>,
    ) {
    }

    fn init_state(world: &mut World) -> Self::State {
        Q::init_state(world)
    }

    fn matches_component_set(
        state: &Self::State,
        set_contains_id: &impl Fn(ComponentId) -> bool,
    ) -> bool {
        Q::matches_component_set(state, set_contains_id)
    }
}

/// SAFETY: `Self::ReadOnly` is `Self`
unsafe impl<Q: WorldQueryData> WorldQueryData for NopWorldQuery<Q> {
    type ReadOnly = Self;

    fn shrink<'wlong: 'wshort, 'wshort>(_: ()) {}
}

/// SAFETY: `NopFetch` never accesses any data
unsafe impl<Q: WorldQueryData> ReadOnlyWorldQueryData for NopWorldQuery<Q> {}

/// SAFETY: `PhantomData` never accesses any world data.
unsafe impl<T: ?Sized> WorldQuery for PhantomData<T> {
    type Item<'a> = ();
    type Fetch<'a> = ();

    type State = ();

    unsafe fn init_fetch<'w>(
        _world: UnsafeWorldCell<'w>,
        _state: &Self::State,
        _last_run: Tick,
        _this_run: Tick,
    ) -> Self::Fetch<'w> {
    }

    unsafe fn clone_fetch<'w>(_fetch: &Self::Fetch<'w>) -> Self::Fetch<'w> {}

    // `PhantomData` does not match any components, so all components it matches
    // are stored in a Table (vacuous truth).
    const IS_DENSE: bool = true;

    unsafe fn set_archetype<'w>(
        _fetch: &mut Self::Fetch<'w>,
        _state: &Self::State,
        _archetype: &'w Archetype,
        _table: &'w Table,
    ) {
    }

    unsafe fn set_table<'w>(_fetch: &mut Self::Fetch<'w>, _state: &Self::State, _table: &'w Table) {
    }

    unsafe fn fetch<'w>(
        _fetch: &mut Self::Fetch<'w>,
        _entity: Entity,
        _table_row: TableRow,
    ) -> Self::Item<'w> {
    }

    fn update_component_access(_state: &Self::State, _access: &mut FilteredAccess<ComponentId>) {}

    fn update_archetype_component_access(
        _state: &Self::State,
        _archetype: &Archetype,
        _access: &mut Access<ArchetypeComponentId>,
    ) {
    }

    fn init_state(_world: &mut World) -> Self::State {}

    fn matches_component_set(
        _state: &Self::State,
        _set_contains_id: &impl Fn(ComponentId) -> bool,
    ) -> bool {
        true
    }
}

/// SAFETY: `PhantomData` never accesses any world data.
unsafe impl<T: ?Sized> WorldQueryData for PhantomData<T> {
    type ReadOnly = Self;

    fn shrink<'wlong: 'wshort, 'wshort>(_item: Self::Item<'wlong>) -> Self::Item<'wshort> {}
}

/// SAFETY: `PhantomData` never accesses any world data.
unsafe impl<T: ?Sized> ReadOnlyWorldQueryData for PhantomData<T> {}

#[cfg(test)]
mod tests {
    use bevy_ecs_macros::WorldQueryData;

    use super::*;
    use crate::{
        self as bevy_ecs,
        system::{assert_is_system, Query},
    };

    #[derive(Component)]
    pub struct A;

    #[derive(Component)]
    pub struct B;

    // Tests that each variant of struct can be used as a `WorldQuery`.
    #[test]
    fn world_query_struct_variants() {
        #[derive(WorldQueryData)]
        pub struct NamedQuery {
            id: Entity,
            a: &'static A,
        }

        #[derive(WorldQueryData)]
        pub struct TupleQuery(&'static A, &'static B);

        #[derive(WorldQueryData)]
        pub struct UnitQuery;

        fn my_system(_: Query<(NamedQuery, TupleQuery, UnitQuery)>) {}

        assert_is_system(my_system);
    }

    // Compile test for https://github.com/bevyengine/bevy/pull/8030.
    #[test]
    fn world_query_phantom_data() {
        #[derive(WorldQueryData)]
        pub struct IgnoredQuery<Marker> {
            id: Entity,
            _marker: PhantomData<Marker>,
        }

        fn ignored_system(_: Query<IgnoredQuery<()>>) {}

        crate::system::assert_is_system(ignored_system);
    }

    // Ensures that each field of a `WorldQuery` struct's read-only variant
    // has the same visibility as its corresponding mutable field.
    #[test]
    fn read_only_field_visibility() {
        mod private {
            use super::*;

            #[derive(WorldQueryData)]
            #[world_query_data(mutable)]
            pub struct Q {
                pub a: &'static mut A,
            }
        }

        let _ = private::QReadOnly { a: &A };

        fn my_system(query: Query<private::Q>) {
            for q in &query {
                let _ = &q.a;
            }
        }

        crate::system::assert_is_system(my_system);
    }

    // Ensures that metadata types generated by the WorldQuery macro
    // do not conflict with user-defined types.
    // Regression test for https://github.com/bevyengine/bevy/issues/8010.
    #[test]
    fn world_query_metadata_collision() {
        // The metadata types generated would be named `ClientState` and `ClientFetch`,
        // but they should rename themselves to avoid conflicts.
        #[derive(WorldQueryData)]
        pub struct Client<S: ClientState> {
            pub state: &'static S,
            pub fetch: &'static ClientFetch,
        }

        pub trait ClientState: Component {}

        #[derive(Component)]
        pub struct ClientFetch;

        #[derive(Component)]
        pub struct C;

        impl ClientState for C {}

        fn client_system(_: Query<Client<C>>) {}

        crate::system::assert_is_system(client_system);
    }
}
