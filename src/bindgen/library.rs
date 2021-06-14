/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use std::io::Write;
use std::collections::{BTreeMap, HashMap};
use std::fs::File;
use std::path;
use std::fs;

use syn;

use bindgen::cargo::Cargo;
use bindgen::config::{self, Config, Language};
use bindgen::annotation::*;
use bindgen::ir::*;
use bindgen::rust_lib;
use bindgen::utilities::*;
use bindgen::writer::{Source, SourceWriter};


use bindgen::dependency_graph::Item;
pub use bindgen::dependency_graph::DependencyList;
use syn::punctuated::Punctuated;

/// A path ref is used to reference a path value
pub type PathRef = String;

/// A library contains all of the information needed to generate bindings for a rust library.
#[derive(Debug, Clone)]
pub struct Library {
    bindings_crate_name: String,
    pub(crate) config: Config,

    enums: BTreeMap<String, Enum>,
    structs: BTreeMap<String, Struct>,
    opaque_items: BTreeMap<String, OpaqueItem>,
    typedefs: BTreeMap<String, Typedef>,
    pub(crate) functions: Vec<Function>,
}

impl Library {
    fn blank(bindings_crate_name: &str, config: &Config) -> Library {
        Library {
            bindings_crate_name: String::from(bindings_crate_name),
            config: config.clone(),

            enums: BTreeMap::new(),
            structs: BTreeMap::new(),
            opaque_items: BTreeMap::new(),
            typedefs: BTreeMap::new(),
            functions: Vec::new(),
        }
    }

    fn add_std_types(&mut self) {
        let mut add_opaque = |name: &str, generic_params: Vec<&str>| {
            self.opaque_items
                .insert(name.to_owned(),
                        OpaqueItem {
                            name: name.to_owned(),
                            generic_params: generic_params
                                .iter()
                                .map(|x| (*x).to_owned())
                                .collect(),
                            annotations: AnnotationSet::new(),
                            documentation: Documentation::none(),
                        })
        };

        add_opaque("String", vec![]);
        add_opaque("Box", vec!["T"]);
        add_opaque("Rc", vec!["T"]);
        add_opaque("Arc", vec!["T"]);
        add_opaque("Result", vec!["T", "E"]);
        add_opaque("Option", vec!["T"]);
        add_opaque("Vec", vec!["T"]);
        add_opaque("HashMap", vec!["K", "V"]);
        add_opaque("BTreeMap", vec!["K", "V"]);
        add_opaque("HashSet", vec!["T"]);
        add_opaque("BTreeSet", vec!["T"]);
        add_opaque("LinkedList", vec!["T"]);
        add_opaque("VecDeque", vec!["T"]);
    }

    /// Parse the specified crate or source file and load #[repr(C)] types for binding generation.
    pub fn load_src(src: &path::Path, config: &Config) -> Result<Library, String> {
        let mut library = Library::blank("", config);
        library.add_std_types();

        rust_lib::parse_src(src,
                            &mut |crate_name, items| {
                                     library.load_syn_crate_mod(&crate_name, items);
                                 })?;

        Ok(library)
    }

    /// Parse the specified crate or source file and load #[repr(C)] types for binding generation.
    pub fn load_crate(lib: Cargo, config: &Config) -> Result<Library, String> {
        let mut library = Library::blank(lib.binding_crate_name(), config);
        library.add_std_types();

        rust_lib::parse_lib(lib,
                            config.parse.parse_deps,
                            &config.parse.include,
                            &config.parse.exclude,
                            &config.parse.expand,
                            &mut |crate_name, items| {
                                     library.load_syn_crate_mod(&crate_name, items);
                                 })?;

        Ok(library)
    }

    fn load_syn_crate_mod(&mut self, crate_name: &str, items: &Vec<syn::Item>) {
        for item in items {
            match item {
                syn::Item::ForeignMod(ref block) => {
                    self.load_syn_foreign_mod(crate_name, item, block);
                }
                syn::Item::Fn(function) => {
                    self.load_syn_fn(crate_name, function, &function.sig);
                }
                syn::Item::Struct(ref variant) => {
                    self.load_syn_struct(crate_name, variant, &variant.fields, &variant.generics);
                }
                syn::Item::Enum(ref variants) => {
                    self.load_syn_enum(crate_name, variants, &variants.variants, &variants.generics);
                }
                syn::Item::Type(ref ty) => {
                    self.load_syn_ty(crate_name, ty, &ty.ty, &ty.generics);
                }
                _ => {}
            }
        }
    }

    /// Enters a `extern "C" { }` declaration and loads function declarations.
    fn load_syn_foreign_mod(&mut self,
                            crate_name: &str,
                            _item: &syn::Item,
                            block: &syn::ItemForeignMod) {
        if !block.abi.is_c() {
            info!("skip {} - (extern block must be extern C)",
                  crate_name);
            return;
        }

        for foreign_item in &block.items {
            match foreign_item {
                syn::ForeignItem::Fn(ref decl) => {
                    if crate_name != self.bindings_crate_name {
                        info!("skip {}::{} - (fn's outside of the binding crate are not used)",
                              crate_name,
                              &decl.sig.ident);
                        return;
                    }

                    let annotations = match AnnotationSet::parse(decl.get_doc_attr()) {
                        Ok(x) => x,
                        Err(msg) => {
                            warn!("{}", msg);
                            AnnotationSet::new()
                        }
                    };

                    match Function::load(decl.sig.ident.to_string(),
                                         annotations,
                                         &decl.sig,
                                         true,
                                         decl.get_doc_attr()) {
                        Ok(func) => {
                            info!("take {}::{}", crate_name, &decl.sig.ident);

                            self.functions.push(func);
                        }
                        Err(msg) => {
                            error!("Cannot use fn {}::{} ({})",
                                   crate_name,
                                   &decl.sig.ident,
                                   msg);
                        }
                    }
                }
                _ => {}
            }
        }
    }

    /// Loads a `fn` declaration
    fn load_syn_fn(&mut self,
                   crate_name: &str,
                   item: &syn::ItemFn,
                   function_sig: &syn::Signature
    ) {
        if crate_name != self.bindings_crate_name {
            info!("skip {}::{} - (fn's outside of the binding crate are not used)",
                  crate_name,
                  &function_sig.ident);
            return;
        }

        if item.is_no_mangle() && function_sig.abi.is_c() {
            let annotations = match AnnotationSet::parse(item.get_doc_attr()) {
                Ok(x) => x,
                Err(msg) => {
                    warn!("{}", msg);
                    AnnotationSet::new()
                }
            };

            match Function::load(function_sig.ident.to_string(),
                                 annotations,
                                 function_sig,
                                 false,
                                 item.get_doc_attr()) {
                Ok(func) => {
                    info!("take {}::{}", crate_name, &function_sig.ident);

                    self.functions.push(func);
                }
                Err(msg) => {
                    error!("cannot use fn {}::{} ({})", crate_name, &function_sig.ident, msg);
                }
            }
        } else {
            if item.is_no_mangle() != function_sig.abi.is_c() {
                warn!("skip {}::{} - (not both `no_mangle` and `extern \"C\"`)",
                      crate_name,
                      &function_sig.ident);
            }
        }
    }

    /// Loads a `struct` declaration
    fn load_syn_struct(&mut self,
                       crate_name: &str,
                       item: &syn::ItemStruct,
                       variant: &syn::Fields,
                       generics: &syn::Generics) {
        let struct_name = item.ident.to_string();
        let annotations = match AnnotationSet::parse(item.get_doc_attr()) {
            Ok(x) => x,
            Err(msg) => {
                warn!("{}", msg);
                AnnotationSet::new()
            }
        };

        if item.is_repr_c() {
            match Struct::load(struct_name.clone(),
                               annotations.clone(),
                               variant,
                               generics,
                               item.get_doc_attr()) {
                Ok(st) => {
                    info!("take {}::{}", crate_name, &item.ident);
                    self.structs.insert(struct_name, st);
                }
                Err(msg) => {
                    info!("take {}::{} - opaque ({})", crate_name, &item.ident, msg);
                    self.opaque_items
                        .insert(struct_name.clone(),
                                OpaqueItem::new(struct_name,
                                                generics,
                                                annotations,
                                                item.get_doc_attr()));
                }
            }
        } else {
            info!("take {}::{} - opaque (not marked as repr(C))",
                  crate_name,
                  &item.ident);
            self.opaque_items
                .insert(struct_name.clone(),
                        OpaqueItem::new(struct_name, generics, annotations, item.get_doc_attr()));
        }
    }

    /// Loads a `enum` declaration
    fn load_syn_enum(&mut self,
                     crate_name: &str,
                     item: &syn::ItemEnum,
                     variants: &Punctuated<syn::Variant, syn::Token![,]>,
                     generics: &syn::Generics) {
        if !generics.params.is_empty() ||
           generics.where_clause.is_some() {
            info!("skip {}::{} - (has generics or lifetimes or where bounds)",
                  crate_name,
                  &item.ident);
            return;
        }

        let enum_name = item.ident.to_string();
        let annotations = match AnnotationSet::parse(item.get_doc_attr()) {
            Ok(x) => x,
            Err(msg) => {
                warn!("{}", msg);
                AnnotationSet::new()
            }
        };

        match Enum::load(enum_name.clone(),
                         item.get_repr(),
                         annotations.clone(),
                         variants,
                         item.get_doc_attr()) {
            Ok(en) => {
                info!("take {}::{}", crate_name, &item.ident);
                self.enums.insert(enum_name, en);
            }
            Err(msg) => {
                info!("take {}::{} - opaque ({})", crate_name, &item.ident, msg);
                self.opaque_items
                    .insert(enum_name.clone(),
                            OpaqueItem::new(enum_name, generics, annotations, item.get_doc_attr()));
            }
        }
    }

    /// Loads a `type` declaration
    fn load_syn_ty(&mut self,
                   crate_name: &str,
                   item: &syn::ItemType,
                   ty: &syn::Type,
                   generics: &syn::Generics) {
        let alias_name = item.ident.to_string();
        let annotations = match AnnotationSet::parse(item.get_doc_attr()) {
            Ok(x) => x,
            Err(msg) => {
                warn!("{}", msg);
                AnnotationSet::new()
            }
        };


        let fail = match Typedef::load(alias_name.clone(),
                                       annotations.clone(),
                                       generics,
                                       ty,
                                       item.get_doc_attr()) {
            Ok(typedef) => {
                info!("take {}::{}", crate_name, &item.ident);
                self.typedefs.insert(alias_name, typedef);
                return;
            }
            Err(msg) => msg,
        };

        info!("skip {}::{} - ({})", crate_name, &item.ident, fail);
    }

    pub fn resolve_path(&self, p: &PathRef) -> Option<Item> {
        if let Some(x) = self.enums.get(p) {
            return Some(Item::Enum(x.clone()));
        }
        if let Some(x) = self.structs.get(p) {
            return Some(Item::Struct(x.clone()));
        }
        if let Some(x) = self.opaque_items.get(p) {
            return Some(Item::Opaque(x.clone()));
        }
        if let Some(x) = self.typedefs.get(p) {
            return Some(Item::Typedef(x.clone()));
        }
        None
    }

    /// Build a bindings file from this rust library.
    pub fn generate(mut self) -> Result<GeneratedBindings, String> {
        let mut result = GeneratedBindings::blank(&self.config);

        // Transfer all typedef annotations to the type they alias
        let mut typedef_annotations = HashMap::new();
        for (_, ref mut typedef) in &mut self.typedefs {
            typedef.transfer_annotations(&mut typedef_annotations);
        }
        for (alias_path, annotations) in typedef_annotations.drain() {
            // TODO
            if let Some(x) = self.enums.get_mut(&alias_path) {
                if !x.annotations.is_empty() {
                    warn!("can't transfer annotations from typedef to alias ({}) that already has annotations.",
                          alias_path);
                    continue;
                }
                x.annotations = annotations;
                continue;
            }
            if let Some(x) = self.structs.get_mut(&alias_path) {
                if !x.annotations.is_empty() {
                    warn!("can't transfer annotations from typedef to alias ({}) that already has annotations.",
                          alias_path);
                    continue;
                }
                x.annotations = annotations;
                continue;
            }
            if let Some(x) = self.opaque_items.get_mut(&alias_path) {
                if !x.annotations.is_empty() {
                    warn!("can't transfer annotations from typedef to alias ({}) that already has annotations.",
                          alias_path);
                    continue;
                }
                x.annotations = annotations;
                continue;
            }
            if let Some(x) = self.typedefs.get_mut(&alias_path) {
                if !x.annotations.is_empty() {
                    warn!("can't transfer annotations from typedef to alias ({}) that already has annotations.",
                          alias_path);
                    continue;
                }
                x.annotations = annotations;
                continue;
            }
        }

        let mut deps = DependencyList::new(&self.functions, &self);
        if self.config.structure.generate_member_functions &&
           self.config.language == Language::Cxx {
            deps.fix_abi();
        }
        // Gather only the items that we need for this
        // `extern "c"` interface
        result.items = deps.calculate_order();
        Ok(result)
    }
}

/// A GeneratedBindings is a completed bindings file ready to be written.
#[derive(Debug, Clone)]
pub struct GeneratedBindings {
    config: Config,
    items: Vec<Item>,
}

impl GeneratedBindings {
    fn blank(config: &Config) -> GeneratedBindings {
        GeneratedBindings {
            config: config.clone(),
            items: Vec::new(),
        }
    }

    pub fn write_to_file(&self, path: &str) {
        if let Some(parent) = path::Path::new(path).parent() {
            fs::create_dir_all(parent).unwrap();
        }

        self.write(File::create(path).unwrap());
    }

    pub fn write<F: Write>(&self, file: F) {
        let mut out = SourceWriter::new(file, &self.config);

        if let Some(ref f) = self.config.header {
            out.new_line_if_not_start();
            out.write(&f);
            out.new_line();
        }
        if let Some(ref f) = self.config.include_guard {
            out.new_line_if_not_start();
            out.write(&format!("#ifndef {}", f));
            out.new_line();
            out.write(&format!("#define {}", f));
            out.new_line();
        }
        if self.config.include_version {
            out.new_line_if_not_start();
            out.write(&format!("/* Generated with cbindgen:{} */", config::VERSION));
            out.new_line();
        }
        if let Some(ref f) = self.config.autogen_warning {
            out.new_line_if_not_start();
            out.write(&f);
            out.new_line();
        }

        out.new_line_if_not_start();
        if self.config.language == Language::C {
            out.write("#include <stdint.h>");
            out.new_line();
            out.write("#include <stdlib.h>");
            out.new_line();
            out.write("#include <stdbool.h>");
        } else {
            out.write("#include <cstdint>");
            out.new_line();
            out.write("#include <cstdlib>");
        }
        out.new_line();

        if self.config.language == Language::Cxx {
            out.new_line_if_not_start();

            let mut wrote_namespace: bool = false;
            if let Some(ref namespace) = self.config.namespace {
                wrote_namespace = true;

                out.new_line();
                out.write("namespace ");
                out.write(namespace);
                out.write(" {");
            }
            if let Some(ref namespaces) = self.config.namespaces {
                wrote_namespace = true;
                for namespace in namespaces {
                    out.new_line();
                    out.write("namespace ");
                    out.write(namespace);
                    out.write(" {");
                }
            }
            if wrote_namespace {
                out.new_line();
            }
        }

        if let Some(ref f) = self.config.autogen_warning {
            out.new_line_if_not_start();
            out.write(&f);
            out.new_line();
        }

        for item in &self.items {
            out.new_line_if_not_start();
            item.write(&self.config, &mut out);
            out.new_line();
        }

        if self.config.language == Language::Cxx {
            let mut wrote_namespace: bool = false;
            if let Some(ref namespaces) = self.config.namespaces {
                wrote_namespace = true;

                for namespace in namespaces.iter().rev() {
                    out.new_line_if_not_start();
                    out.write("} // namespace ");
                    out.write(namespace);
                }
            }
            if let Some(ref namespace) = self.config.namespace {
                wrote_namespace = true;

                out.new_line_if_not_start();
                out.write("} // namespace ");
                out.write(namespace);
            }
            if wrote_namespace {
                out.new_line();
            }
        }

        if self.config.language == Language::Cxx {
            out.new_line_if_not_start();
            out.write("static_assert(sizeof(float) == 4, \"Float size does not match\");");
            out.new_line();
            out.write("static_assert(sizeof(double) == 8, \"Double size does not match\");");
            out.new_line();
        }

        if let Some(ref f) = self.config.autogen_warning {
            out.new_line_if_not_start();
            out.write(&f);
            out.new_line();
        }
        if let Some(ref f) = self.config.include_guard {
            out.new_line_if_not_start();
            out.write(&format!("#endif // {}", f));
            out.new_line();
        }
        if let Some(ref f) = self.config.trailer {
            out.new_line_if_not_start();
            out.write(&f);
            out.new_line();
        }
    }
}
