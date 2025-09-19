mod bevy_app_impl;
mod export_component_impl;
mod export_entity_impl;

use proc_macro::TokenStream;

#[proc_macro_attribute]
pub fn bevy_app(attr: TokenStream, item: TokenStream) -> TokenStream {
    bevy_app_impl::expand(attr, item)
}


#[proc_macro_derive(ExportComponent, attributes(export_component, export))]
pub fn export_component(input: TokenStream) -> TokenStream {
    export_component_impl::expand(input)
}

#[proc_macro_derive(ExportEntity, attributes(export_entity))]
pub fn export_entity(input: TokenStream) -> TokenStream {
    export_entity_impl::expand(input)
}