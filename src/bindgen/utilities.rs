/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use bindgen::ir::*;
use syn::{
    Abi, Attribute, Field, ForeignItemFn, GenericArgument, ItemEnum, ItemFn, ItemStruct, ItemType,
    Lit, Meta, PathArguments, TypePath, Variant,
};

pub trait IterHelpers: Iterator {
    fn try_skip_map<F, T, E>(&mut self, f: F) -> Result<Vec<T>, E>
    where
        F: FnMut(&Self::Item) -> Result<Option<T>, E>;
}

impl<I> IterHelpers for I
where
    I: Iterator,
{
    fn try_skip_map<F, T, E>(&mut self, mut f: F) -> Result<Vec<T>, E>
    where
        F: FnMut(&Self::Item) -> Result<Option<T>, E>,
    {
        let mut out = Vec::new();
        while let Some(item) = self.next() {
            if let Some(x) = f(&item)? {
                out.push(x);
            }
        }
        Ok(out)
    }
}

pub fn find_first_some<T>(slice: &[Option<T>]) -> Option<&T> {
    for x in slice {
        if let &Some(ref x) = x {
            return Some(x);
        }
    }
    return None;
}

pub trait SynItemHelpers {
    fn attrs(&self) -> &[Attribute];

    fn has_attr(&self, target: Attribute) -> bool {
        self.attrs().iter().any(|a| a == &target)
    }

    fn get_doc_attr(&self) -> String {
        self.attrs()
            .iter()
            .filter_map(|a| {
                let meta = a.parse_meta().ok()?;
                if let Meta::NameValue(meta) = meta {
                    if let Some(ident) = meta.path.get_ident() {
                        if ident == "doc" {
                            if let Lit::Str(lit) = meta.lit {
                                Some(lit.value() + "\n")
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
            .collect()
    }

    fn is_no_mangle(&self) -> bool {
        self.has_attr(syn::parse_quote!(#[no_mangle]))
    }

    fn is_repr_c(&self) -> bool {
        let repr_attribute = syn::parse_quote!(#[repr(C)]);
        self.has_attr(repr_attribute)
    }

    fn is_repr_u32(&self) -> bool {
        let repr_attribute = syn::parse_quote!(#[repr(u32)]);
        self.has_attr(repr_attribute)
    }

    fn is_repr_u16(&self) -> bool {
        let repr_attribute = syn::parse_quote!(#[repr(u16)]);
        self.has_attr(repr_attribute)
    }

    fn is_repr_u8(&self) -> bool {
        let repr_attribute = syn::parse_quote!(#[repr(u8)]);
        self.has_attr(repr_attribute)
    }

    fn get_repr(&self) -> Repr {
        if self.is_repr_c() {
            Repr::C
        } else if self.is_repr_u32() {
            Repr::U32
        } else if self.is_repr_u16() {
            Repr::U16
        } else if self.is_repr_u8() {
            Repr::U8
        } else {
            Repr::None
        }
    }
}

impl SynItemHelpers for ItemStruct {
    fn attrs(&self) -> &[Attribute] {
        &self.attrs
    }
}

impl SynItemHelpers for ItemEnum {
    fn attrs(&self) -> &[Attribute] {
        &self.attrs
    }
}

impl SynItemHelpers for ItemType {
    fn attrs(&self) -> &[Attribute] {
        &self.attrs
    }
}

impl SynItemHelpers for ItemFn {
    fn attrs(&self) -> &[Attribute] {
        &self.attrs
    }
}

impl SynItemHelpers for Variant {
    fn attrs(&self) -> &[Attribute] {
        &self.attrs
    }
}

impl SynItemHelpers for Field {
    fn attrs(&self) -> &[Attribute] {
        &self.attrs
    }
}

impl SynItemHelpers for ForeignItemFn {
    fn attrs(&self) -> &[Attribute] {
        &self.attrs
    }
}

/// Helper function for accessing Abi information
pub trait SynAbiHelpers {
    fn is_c(&self) -> bool;
}

impl SynAbiHelpers for Option<Abi> {
    fn is_c(&self) -> bool {
        self.as_ref().map(|abi| abi.is_c()).unwrap_or(false)
    }
}

impl SynAbiHelpers for Abi {
    fn is_c(&self) -> bool {
        self.name
            .as_ref()
            .map(|l| l.value() == "C")
            .unwrap_or(false)
    }
}

/// Helper function for loading a Path and generics from a syn::Path
pub trait SynPathHelpers {
    fn convert_to_generic_single_segment(&self) -> Result<(String, Vec<Type>), String>;
}

impl SynPathHelpers for TypePath {
    fn convert_to_generic_single_segment(&self) -> Result<(String, Vec<Type>), String> {
        if self.path.segments.len() != 1 {
            return Err(format!("path contains more than one segment"));
        }

        let name = self.path.segments[0].ident.to_string();

        if name == "PhantomData" {
            return Ok((name, Vec::new()));
        }

        let generics = match &self.path.segments[0].arguments {
            &PathArguments::AngleBracketed(ref d) => d.args.iter().try_skip_map(|x| match x {
                GenericArgument::Type(x) => Type::load(x),
                _ => Err(format!(
                    "path generic parameter contains bindings, or lifetimes"
                )),
            })?,
            &PathArguments::Parenthesized(_) => {
                return Err(format!("path contains parentheses"));
            }
            PathArguments::None => Vec::new(),
        };

        Ok((name, generics))
    }
}
