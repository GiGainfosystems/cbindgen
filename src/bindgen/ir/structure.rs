/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use std::fmt::{self, Display};
use std::io::Write;

use syn::{self, GenericParam};

use bindgen::annotation::*;
use bindgen::cdecl;
use bindgen::config::{Config, Language};
use bindgen::dependency_graph::{DependencyKind, Item};
use bindgen::ir::*;
use bindgen::library::*;
use bindgen::rename::*;
use bindgen::utilities::*;
use bindgen::writer::*;

#[derive(Debug, Clone)]
pub struct StructField {
    pub name: String,
    pub tpe: Type,
    pub doc: Documentation,
}

impl StructField {
    pub fn specialize(&self, mappings: &[(&String, &Type)]) -> Self {
        StructField {
            tpe: self.tpe.specialize(mappings),
            ..self.clone()
        }
    }
}

impl Source for StructField {
    fn write<F: Write>(&self, config: &Config, out: &mut SourceWriter<F>) {
        self.doc.write(config, out);
        cdecl::write_field(out, &self.tpe, &self.name);
    }
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub name: String,
    pub annotations: AnnotationSet,
    pub fields: Vec<StructField>,
    pub generic_params: Vec<String>,
    pub documentation: Documentation,
    pub specialization: Option<Specialization>,
    pub intern: bool,
}

impl Display for Struct {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Struct {}", self.name)
    }
}

impl Struct {
    pub fn load(
        name: String,
        annotations: AnnotationSet,
        decl: &syn::Fields,
        generics: &syn::Generics,
        doc: String,
    ) -> Result<Struct, String> {
        let fields = match decl {
            &syn::Fields::Named(ref fields) => {
                fields.named.iter().try_skip_map(|x| x.as_struct_field())?
            }
            &syn::Fields::Unnamed(ref fields) => {
                let mut out = Vec::new();
                let mut current = 0;
                for field in &fields.unnamed {
                    if let Some(tpe) = Type::load(&field.ty)? {
                        out.push(StructField {
                            name: format!("_{}", current),
                            tpe,
                            doc: Documentation::load(field.get_doc_attr()),
                        });
                        current += 1;
                    }
                }
                out
            }
            &syn::Fields::Unit => vec![],
        };

        let generic_params = generics
            .params
            .iter()
            .map(|x| match x {
                GenericParam::Type(x) => x.ident.to_string(),
                _ => unimplemented!("Only types allowed here"),
            })
            .collect::<Vec<_>>();

        Ok(Struct {
            name: name,
            annotations: annotations,
            fields: fields,
            generic_params: generic_params,
            documentation: Documentation::load(doc),
            specialization: None,
            intern: false,
        })
    }

    pub fn as_opaque(&self) -> OpaqueItem {
        OpaqueItem {
            name: self.name.clone(),
            generic_params: self.generic_params.clone(),
            annotations: self.annotations.clone(),
            documentation: self.documentation.clone(),
        }
    }

    pub fn get_deps(&self, library: &Library) -> Vec<(Item, DependencyKind)> {
        let mut ret = Vec::new();
        for f in &self.fields {
            ret.extend_from_slice(&f.tpe.get_items(library, DependencyKind::Normal));
        }
        if let Some(ref s) = self.specialization {
            ret.push((Item::Specialization(s.clone()), DependencyKind::Normal));
        }
        ret
    }

    pub fn rename_fields(&mut self, config: &Config) {
        let rules = [
            self.annotations.parse_atom::<RenameRule>("rename-all"),
            config.structure.rename_fields,
        ];

        if let Some(o) = self.annotations.list("field-names") {
            let mut overriden_fields = Vec::<StructField>::new();

            for (i, ref s) in self.fields.iter().enumerate() {
                if i >= o.len() {
                    overriden_fields.push((*s).clone());
                } else {
                    overriden_fields.push(StructField {
                        name: o[i].clone(),
                        ..(*s).clone()
                    });
                }
            }

            self.fields = overriden_fields;
        } else if let Some(r) = find_first_some(&rules) {
            self.fields = self
                .fields
                .iter()
                .map(|x| StructField {
                    name: r.apply_to_snake_case(&x.name, IdentifierType::StructMember),
                    ..x.clone()
                })
                .collect();
        }
    }

    pub fn mangle_paths(&mut self) {
        for &mut StructField { ref mut tpe, .. } in &mut self.fields {
            tpe.mangle_paths();
        }
    }
}

impl Source for Struct {
    fn write<F: Write>(&self, config: &Config, out: &mut SourceWriter<F>) {
        assert!(self.generic_params.is_empty());
        if config.language == Language::Cxx && self.intern {
            out.write("namespace intern");
            out.open_brace();
        }
        self.documentation.write(config, out);
        if config.language == Language::C {
            out.write("typedef struct");
        } else {
            out.write(&format!("extern \"C\" struct {}", self.name));
        }
        out.open_brace();

        out.write_vertical_source_list(&self.fields, ListType::Cap(";"));

        out.close_brace(config.language != Language::C);
        if config.language == Language::C {
            out.write(&format!(" {};", self.name));
        }
        if config.language == Language::Cxx && self.intern {
            out.close_brace(false);
        }
    }
}

pub trait SynFieldHelpers {
    fn as_struct_field(&self) -> Result<Option<StructField>, String>;
}

impl SynFieldHelpers for syn::Field {
    fn as_struct_field(&self) -> Result<Option<StructField>, String> {
        let ident = self
            .ident
            .as_ref()
            .ok_or(format!("field is missing identifier"))?
            .clone();
        let converted_ty = Type::load(&self.ty)?;

        if let Some(tpe) = converted_ty {
            Ok(Some(StructField {
                name: ident.to_string(),
                tpe,
                doc: Documentation::load(self.get_doc_attr()),
            }))
        } else {
            Ok(None)
        }
    }
}
