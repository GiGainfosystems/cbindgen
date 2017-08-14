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
use bindgen::mangle;

/// A type alias that generates a copy of its aliasee with a new name. If the type
/// alias has generic values, it monomorphosizes its aliasee. This is useful for
/// presenting an interface that includes generic types.
#[derive(Debug, Clone)]
pub struct Specialization {
    pub name: String,
    pub annotations: AnnotationSet,
    pub generic_params: Vec<String>,
    pub generic_values: Vec<Type>,
    pub documentation: Documentation,
}

impl Specialization {
    pub fn get_deps(&self, library: &Library) -> Vec<(Item, DependencyKind)> {
        if self.generic_values.is_empty() {
            return Vec::new();
        }
        if let Some(v) = library.resolve_path(&self.name) {
            let mut ret = self.generic_values.iter()
                .flat_map(|g| g.get_items(library, DependencyKind::Normal))
                .collect::<Vec<_>>();
            match v {
                Item::Struct(mut s) => {
                    s.name = mangle::mangle_path(&s.name, &self.generic_values);
                    ret.push((Item::Specialization(Specialization {
                        generic_values: Vec::new(),
                        ..self.clone()
                    }), DependencyKind::Normal));
                    ret
                }
                Item::Typedef(mut t) => {
                    t.name = mangle::mangle_path(&t.name, &self.generic_values);
                    ret.push((Item::Specialization(Specialization {
                        generic_values: Vec::new(),
                        ..self.clone()
                    }), DependencyKind::Normal));
                    ret
                }
                e =>{ println!("{:?}", e); unimplemented!()}
            }
        } else {
            Vec::new()
        }
    }
}

impl Source for Specialization {
    fn write<F: Write>(&self, config: &Config, out: &mut SourceWriter<F>) {
        self.documentation.write(config, out);
        if self.generic_values.is_empty() {
            out.write("template<");
            let mut first = true;
            for t in &self.generic_params {
                if first {
                    first = false;
                } else {
                    out.write(", ");
                }
                out.write(&format!("typename {}", t));
            }
            out.write(">");
            out.new_line();
            out.write(&format!("struct {};", self.name));
        } else {
            out.write("template<>");
            out.new_line();
            out.write(&format!("struct {}<", self.name));
            let mut first = true;
            for t in &self.generic_values {
                if first {
                    first = false;
                } else {
                    out.write(", ");
                }
                t.write(config, out);
            }
            out.write(&format!("> : public {}", &mangle::mangle_path(&self.name, &self.generic_values)));
            out.open_brace();
            out.close_brace(true);
        }
    }
}

/// A type alias that is represented as a C typedef
#[derive(Debug, Clone)]
pub struct Typedef {
    pub name: String,
    pub annotations: AnnotationSet,
    pub generic_params: Vec<String>,
    pub generic_values: Vec<Type>,
    pub aliased: Type,
    pub documentation: Documentation,
    pub specialization: Option<Specialization>,
}

impl Typedef {
    pub fn load(name: String,
                annotations: AnnotationSet,
                generics: &syn::Generics,
                ty: &syn::Ty,
                doc: String)
                -> Result<Typedef, String> {
        if let Some(x) = Type::load(ty)? {
            match ty {
                &syn::Ty::Path(_, ref p) => {
                   let generic_params = generics
                        .ty_params
                        .iter()
                        .map(|x| x.ident.to_string())
                        .collect::<Vec<_>>();

                    let (_, generic_values) = p.convert_to_generic_single_segment()?;
                    Ok(Typedef {
                        name: name,
                        annotations: annotations,
                        aliased: x,
                        generic_params,
                        generic_values,
                        documentation: Documentation::load(doc),
                        specialization: None,
                    })
                }
                _ if generics.ty_params.is_empty() &&
                    generics.lifetimes.is_empty() => {
                    Ok(Typedef {
                        name: name,
                        annotations: annotations,
                        aliased: x,
                        generic_params: Vec::new(),
                        generic_values: Vec::new(),
                        documentation: Documentation::load(doc),
                        specialization: None,
                    })
                }
                i => {
                    println!("{:?}", i);
                    unimplemented!()
                }
            }
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
                          alias_path,
                          self.name);
                    return;
                }

                out.insert(alias_path, self.annotations.clone());
                self.annotations = AnnotationSet::new();
            }
            None => {}
        }
    }

    pub fn mangle_paths(&mut self) {
        self.aliased.mangle_paths();
    }

    pub fn get_deps(&self, library: &Library) -> Vec<(Item, DependencyKind)> {
        assert!(self.generic_params.is_empty());
        let mut ret = self.aliased.get_items(library, DependencyKind::Normal);
        if let Some(ref s) = self.specialization {
            ret.push((Item::Specialization(s.clone()), DependencyKind::Normal));
        }
        ret
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
