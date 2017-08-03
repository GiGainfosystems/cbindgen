/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use std::collections::BTreeMap;
use std::io::Write;

use syn;

use bindgen::annotation::*;
use bindgen::config::{Config, Language};
use bindgen::ir::*;
use bindgen::library::*;
use bindgen::mangle::*;
use bindgen::writer::*;

#[derive(Debug, Clone)]
pub struct OpaqueItem {
    pub name: PathRef,
    pub generic_params: Vec<String>,
    pub annotations: AnnotationSet,
    pub documentation: Documentation,
}

impl OpaqueItem {
    pub fn new(name: String,
               generics: &syn::Generics,
               annotations: AnnotationSet,
               doc: String) -> OpaqueItem {
        let generic_params = generics.ty_params.iter()
                                               .map(|x| x.ident.to_string())
                                               .collect::<Vec<_>>();

        OpaqueItem {
            name: name,
            generic_params: generic_params,
            annotations: annotations,
            documentation: Documentation::load(doc),
        }
    }

    pub fn add_monomorphs(&self, generic_values: &Vec<Type>, out: &mut Monomorphs) {
        assert!(self.generic_params.len() == generic_values.len());

        if self.generic_params.len() == 0 {
            return;
        }

        let monomorph = OpaqueItem {
            name: mangle_path(&self.name, generic_values),
            generic_params: vec![],
            annotations: self.annotations.clone(),
            documentation: self.documentation.clone(),
        };

        if !out.contains_key(&self.name) {
            out.insert(self.name.clone(), BTreeMap::new());
        }
        out.get_mut(&self.name).unwrap().insert(generic_values.clone(), 
                                                Monomorph::OpaqueItem(monomorph));
    }
}

impl Source for OpaqueItem {
    fn write<F: Write>(&self, config: &Config, out: &mut SourceWriter<F>) {
        self.documentation.write(config, out);
        if config.language == Language::C {
            out.write(&format!("struct {};", self.name));
            out.new_line();
            out.write(&format!("typedef struct {} {};", self.name, self.name));
        } else {
            out.write(&format!("struct {};", self.name));
        }
    }
}
