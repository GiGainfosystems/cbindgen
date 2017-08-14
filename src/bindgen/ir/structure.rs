/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use std::io::Write;
use std::fmt::{Display, self};

use syn;

use bindgen::annotation::*;
use bindgen::dependency_graph::{Item, DependencyKind};
use bindgen::config::{Config, Language, Layout};
use bindgen::ir::*;
use bindgen::library::*;
use bindgen::rename::*;
use bindgen::utilities::*;
use bindgen::writer::*;
use bindgen::cdecl;
use bindgen::mangle;

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
    pub functions: Vec<Function>,
    pub destructor: Option<Function>,
    pub specialization: Option<Specialization>,
}

impl Display for Struct {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Struct {}", self.name)
    }
}

impl Struct {
    pub fn load(name: String,
                annotations: AnnotationSet,
                decl: &syn::VariantData,
                generics: &syn::Generics,
                doc: String) -> Result<Struct, String>
    {
        let fields = match decl {
            &syn::VariantData::Struct(ref fields) => {
                fields.iter()
                      .try_skip_map(|x| x.as_struct_field())?
            }
            &syn::VariantData::Tuple(ref fields) => {
                let mut out = Vec::new();
                let mut current = 0;
                for field in fields {
                    if let Some(tpe) = Type::load(&field.ty)? {
                        out.push(StructField{
                            name: format!("_{}", current),
                            tpe,
                            doc: Documentation::load(field.get_doc_attr())
                        });
                        current += 1;
                    }
                }
                out
            }
            &syn::VariantData::Unit => {
                vec![]
            }
        };

        let generic_params = generics.ty_params.iter()
                                               .map(|x| x.ident.to_string())
                                               .collect::<Vec<_>>();

        Ok(Struct {
            name: name,
            annotations: annotations,
            fields: fields,
            generic_params: generic_params,
            documentation: Documentation::load(doc),
            functions: Vec::new(),
            destructor: None,
            specialization: None,
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
        if library.config.structure.generate_member_functions {
            for f in &library.functions {
                if let Some(arg) = f.args.get(0) {
                    match arg.1 {
                        Type::Ptr(ref t) | Type::ConstPtr(ref t) => {
                            match **t {
                                Type::Path(ref p, ref g) => {
                                    let name = mangle::mangle_path(p, g);
                                    if name == self.name {
                                        ret.push((Item::Function(f.clone()),
                                                  DependencyKind::Ptr))
                                    }
                                }
                                _ => {}
                            }
                        }
                        _ => {}
                    }
                }
            }
        }
        ret
    }

    pub fn rename_fields(&mut self, config: &Config) {
        let rules = [self.annotations.parse_atom::<RenameRule>("rename-all"),
                     config.structure.rename_fields];

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
            self.fields = self.fields.iter()
                .map(|x| StructField {
                    name: r.apply_to_snake_case(&x.name, IdentifierType::StructMember),
                    ..x.clone()
                })
                .collect();
        }

        for f in &mut self.functions {
            f.rename_args(config);
        }
        if let Some(ref mut destructor) = self.destructor {
            destructor.rename_args(config);
        }
    }

    pub fn mangle_paths(&mut self) {
        for &mut StructField{ ref mut tpe, ..} in &mut self.fields {
            tpe.mangle_paths();
        }
        for f in &mut self.functions {
            f.mangle_paths();
        }
        if let Some(ref mut destructor) = self.destructor {
            destructor.mangle_paths();
        }
    }

    pub fn add_member_functions(&mut self, functions: Vec<Function>) {
        for function in functions {
            if function.annotations.bool("destructor").unwrap_or(false)
                && self.destructor.is_none() && function.args.len() == 1 &&
                function.ret == Type::Primitive(PrimitiveType::Void)
            {
                self.destructor = Some(function);
            } else if !function.annotations.bool("destructor").unwrap_or(false) {
                self.functions.push(function);
            } else {
                warn!("Found double destructor annotation for struct {}", self.name);
            }
        }
    }

    pub fn write_destructor<F: Write>(&self, config: &Config, out: &mut SourceWriter<F>) {
        if let Some(ref destructor) = self.destructor {
            if !destructor.extern_decl {
                out.new_line();
                out.new_line();
                // Explicitly disable copy constructor and assignment
                out.write(&format!("{0}(const {0}&) = delete;", self.name));
                out.new_line();
                out.write(&format!("{0}& operator=(const {0}&) = delete;", self.name));
                out.new_line();
                out.write(&format!("{0}({0}&&) = default;", self.name));
                out.new_line();
                out.write(&format!("{0}& operator=({0}&&) = default;", self.name));
                out.new_line();
                out.new_line();

                out.write(&format!("~{}()", self.name));
                out.open_brace();
                let option_1 = out.measure(|out| format_function_call_1(destructor, out));

                if (config.function.args == Layout::Auto && option_1 <= config.line_length) ||
                    config.function.args == Layout::Horizontal {
                        format_function_call_1(destructor, out);
                    } else {
                        format_function_call_2(destructor, out);
                    }

                out.close_brace(false);
            }
        }
    }

    pub fn write_functions<F: Write>(&self, config: &Config, out: &mut SourceWriter<F>) {
        if !self.functions.is_empty() {
            out.new_line();
        }
        for f in &self.functions {
            if f.extern_decl {
                continue;
            }
            out.new_line();
            f.write_formated(config, out, FunctionWriteMode::MemberFunction);
            out.open_brace();
            let option_1 = out.measure(|out| format_function_call_1(f, out));

            if (config.function.args == Layout::Auto && option_1 <= config.line_length) ||
                config.function.args == Layout::Horizontal {
                    format_function_call_1(f, out);
                } else {
                    format_function_call_2(f, out);
                }

            out.close_brace(false);
            out.new_line();
        }
    }
}



impl Source for Struct {
    fn write<F: Write>(&self, config: &Config, out: &mut SourceWriter<F>) {
        assert!(self.generic_params.is_empty());

        self.documentation.write(config, out);
        if config.language == Language::C {
            out.write("typedef struct");
        } else {
            out.write(&format!("struct {}", self.name));
        }
        out.open_brace();

        out.write_vertical_source_list(&self.fields, ListType::Cap(";"));

        if config.language == Language::Cxx {
            let mut wrote_start_newline = false;

            let other = if let Some(r) = config.function.rename_args {
                r.apply_to_snake_case("other", IdentifierType::FunctionArg)
            } else {
                String::from("other")
            };

            self.write_destructor(config, out);
            self.write_functions(config, out);

            let mut emit_op = |op, conjuc| {
                if !wrote_start_newline {
                    wrote_start_newline = true;
                    out.new_line();
                }

                out.new_line();

                out.write(&format!("bool operator{}(const {}& {}) const", op, self.name, other));
                out.open_brace();
                out.write("return ");
                out.write_vertical_list(&self.fields.iter()
                                        .map(|x| format!("{0} {1} {2}.{0}",
                                                         x.name, op, other))
                                                    .collect(),
                                        ListType::Join(&format!(" {}", conjuc)));
                out.write(";");
                out.close_brace(false);
            };

            if config.structure.derive_eq(&self.annotations) &&
               !self.fields.is_empty() && self.fields.iter().all(|x| x.tpe.can_cmp_eq()) {
                emit_op("==", "&&");
            }
            if config.structure.derive_neq(&self.annotations) &&
               !self.fields.is_empty() && self.fields.iter().all(|x| x.tpe.can_cmp_eq()) {
                emit_op("!=", "||");
            }
            if config.structure.derive_lt(&self.annotations) &&
               self.fields.len() == 1 && self.fields[0].tpe.can_cmp_order() {
                emit_op("<", "&&");
            }
            if config.structure.derive_lte(&self.annotations) &&
               self.fields.len() == 1 && self.fields[0].tpe.can_cmp_order() {
                emit_op("<=", "&&");
            }
            if config.structure.derive_gt(&self.annotations) &&
               self.fields.len() == 1 && self.fields[0].tpe.can_cmp_order() {
                emit_op(">", "&&");
            }
            if config.structure.derive_gte(&self.annotations) &&
               self.fields.len() == 1 && self.fields[0].tpe.can_cmp_order() {
                emit_op(">=", "&&");
            }
        }

        if config.language == Language::C {
            out.close_brace(false);
            out.write(&format!(" {};", self.name));
        } else {
            out.close_brace(true);
        }
    }
}

fn format_function_call_1<W: Write>(f: &Function, out: &mut SourceWriter<W>) {
    if f.ret == Type::Primitive(PrimitiveType::Void) {
        out.write("::");
    } else {
        out.write("return ::");
    }
    out.write(&f.name);
    out.write("(this");
    for &(ref name, _) in &f.args[1..] {
        out.write(", ");
        out.write(name);
    }
    out.write(");");
}

fn format_function_call_2<W: Write>(f: &Function, out: &mut SourceWriter<W>) {
    if f.ret == Type::Primitive(PrimitiveType::Void) {
        out.write("::");
    } else {
        out.write("return ::");
    }
    out.write(&f.name);
    out.write("(");
    let align_length = out.line_length_for_align();
    out.push_set_spaces(align_length);
    out.write("this");
    for &(ref name, _) in &f.args[1..] {
        out.write(",");
        out.new_line();
        out.write(name);
    }
    out.pop_tab();
    out.write(");");
}

pub trait SynFieldHelpers {
    fn as_struct_field(&self) -> Result<Option<StructField>, String>;
}

impl SynFieldHelpers for syn::Field {
    fn as_struct_field(&self) -> Result<Option<StructField>, String> {
        let ident = self.ident.as_ref().ok_or(format!("field is missing identifier"))?.clone();
        let converted_ty = Type::load(&self.ty)?;

        if let Some(tpe) = converted_ty {
            Ok(Some(StructField{
                name: ident.to_string(),
                tpe,
                doc: Documentation::load(self.get_doc_attr())
            }))
        } else {
            Ok(None)
        }
    }
}
