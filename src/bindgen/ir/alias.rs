/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use std::collections::HashMap;
use std::io::Write;

use syn;

use bindgen::annotation::*;
use bindgen::config::Config;
use bindgen::ir::*;
use bindgen::library::*;
use bindgen::utilities::*;
use bindgen::writer::*;
use bindgen::dependency_graph::{Item, DependencyKind};

/// A type alias that generates a copy of its aliasee with a new name. If the type
/// alias has generic values, it monomorphosizes its aliasee. This is useful for
/// presenting an interface that includes generic types.
#[derive(Debug, Clone)]
pub struct Specialization {
    pub name: String,
    pub annotations: AnnotationSet,
    pub aliased: PathRef,
    pub generic_params: Vec<String>,
    pub generic_values: Vec<Type>,
    pub documentation: Documentation,
}

impl Specialization {
    pub fn load(name: String,
                annotations: AnnotationSet,
                generics: &syn::Generics,
                ty: &syn::Ty,
                doc: String) -> Result<Specialization, String>
    {
        match ty {
            &syn::Ty::Path(ref _q, ref p) => {
                let generic_params = generics.ty_params.iter()
                                                       .map(|x| x.ident.to_string())
                                                       .collect::<Vec<_>>();

                let (path, generic_values) = p.convert_to_generic_single_segment()?;

                if PrimitiveType::maybe(&path).is_some() {
                    return Err(format!("can't specialize a primitive"));
                }

                Ok(Specialization {
                    name: name,
                    annotations: annotations,
                    aliased: path,
                    generic_params: generic_params,
                    generic_values: generic_values,
                    documentation: Documentation::load(doc),
                })
            }
            _ => {
                Err(format!("not a path"))
            }
        }
    }
}

/// A type alias that is represented as a C typedef
#[derive(Debug, Clone)]
pub struct Typedef {
    pub name: String,
    pub annotations: AnnotationSet,
    pub aliased: Type,
    pub documentation: Documentation,
}

impl Typedef {
    pub fn load(name: String,
                annotations: AnnotationSet,
                ty: &syn::Ty,
                doc: String) -> Result<Typedef, String> {
        if let Some(x) = Type::load(ty)? {
            Ok(Typedef {
                name: name,
                annotations: annotations,
                aliased: x,
                documentation: Documentation::load(doc),
            })
        } else {
            Err(format!("cannot have a typedef of a zero sized type"))
        }
    }

    pub fn transfer_annotations(&mut self, out: &mut HashMap<PathRef, AnnotationSet>) {
        if self.annotations.is_empty() {
            return;
        }

        match self.aliased.get_root_path() {
            Some(alias_path) => {
                if out.contains_key(&alias_path) {
                    warn!("multiple typedef's with annotations for {}. ignoring annotations from {}.",
                          alias_path, self.name);
                    return;
                }

                out.insert(alias_path, self.annotations.clone());
                self.annotations = AnnotationSet::new();
            }
            None => { }
        }
    }

    pub fn mangle_paths(&mut self) {
        self.aliased.mangle_paths();
    }

    pub fn get_deps(&self, library: &Library) -> Vec<(Item, DependencyKind)> {
        self.aliased.get_items(library, DependencyKind::Normal)
    }
}

impl Source for Typedef {
    fn write<F: Write>(&self, config: &Config, out: &mut SourceWriter<F>) {
        self.documentation.write(config, out);
        out.write("typedef ");
        (self.name.clone(), self.aliased.clone()).write(config, out);
        out.write(";");
    }
}
