/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use std::io::Write;

use syn;

use bindgen::annotation::*;
use bindgen::cdecl;
use bindgen::config::{Config, Layout, Language};
use bindgen::ir::*;
use bindgen::library::*;
use bindgen::rename::*;
use bindgen::utilities::*;
use bindgen::writer::*;
use bindgen::dependency_graph::{Item, DependencyKind};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum FunctionWriteMode {
    Global,
    Intern,
    Inline,
    MemberFunction,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub annotations: AnnotationSet,
    pub ret: Type,
    pub args: Vec<(String, Type)>,
    pub extern_decl: bool,
    pub documentation: Documentation,
    pub function_type: FunctionWriteMode,
}

impl Function {
    pub fn load(name: String,
                annotations: AnnotationSet,
                decl: &syn::Signature,
                extern_decl: bool,
                doc: String)
                -> Result<Function, String> {
        let args = decl.inputs.iter().try_skip_map(|x| x.as_ident_and_type())?;
        let ret = decl.output.as_type()?;

        Ok(Function {
               name: name,
               annotations: annotations,
               ret: ret,
               args: args,
               extern_decl: extern_decl,
               documentation: Documentation::load(doc),
               function_type: FunctionWriteMode::Global,
           })
    }

    pub fn rename_args(&mut self, config: &Config) {
        let rules = [self.annotations.parse_atom::<RenameRule>("rename-all"),
                     config.function.rename_args];

        if let Some(r) = find_first_some(&rules) {
            self.args = self.args
                .iter()
                .map(|x| (r.apply_to_snake_case(&x.0, IdentifierType::FunctionArg), x.1.clone()))
                .collect()
        }
    }

    pub fn mangle_paths(&mut self) {
        self.ret.mangle_paths();
        for &mut (_, ref mut ty) in &mut self.args {
            ty.mangle_paths();
        }
    }

    pub fn get_deps(&self, library: &Library) -> Vec<(Item, DependencyKind)> {
        let mut ret = self.ret.get_items(library, DependencyKind::Normal);
        for &(_, ref arg) in &self.args {
            ret.extend_from_slice(&arg.get_items(library, DependencyKind::Normal));
        }
        ret
    }

    fn write_function_header<W: Write>(&self,
                                       config: &Config,
                                       out: &mut SourceWriter<W>,
                                       layout_vertical: bool) {
        let prefix = config.function.prefix(&self.annotations);
        let postfix = config.function.postfix(&self.annotations);
        if (self.function_type == FunctionWriteMode::Global ||
            self.function_type == FunctionWriteMode::Intern) &&
           config.language == Language::Cxx {
            out.write("extern \"C\" ");
        } else if self.function_type == FunctionWriteMode::Inline &&
                  config.language == Language::Cxx {
            out.write("inline ");
        }
        if let Some(ref prefix) = prefix {
            out.write(prefix);
            out.write(" ");
        }
        match self.function_type {
            FunctionWriteMode::Global |
            FunctionWriteMode::Inline |
            FunctionWriteMode::Intern => {
                cdecl::write_func(out, self, layout_vertical);
            }
            FunctionWriteMode::MemberFunction => {
                let f = Function {
                    args: self.args[1..].to_owned(),
                    ..self.clone()
                };
                cdecl::write_func(out, &f, layout_vertical);
                if let Type::ConstPtr(_) = self.args[0].1 {
                    out.write(" const");
                }
            }
        }
        if let Some(ref postfix) = postfix {
            out.write(" ");
            out.write(postfix);
        }
        if self.function_type == FunctionWriteMode::Global ||
           self.function_type == FunctionWriteMode::Intern {
            out.write(";");
        }
    }

    fn get_namespace(&self, config: &Config) -> String {
        let mut namespace = String::new();
        if let Some(ref n) = config.namespace {
            namespace += n;
        }
        if let Some(ref namespaces) = config.namespaces {
            let mut has_namespace = !namespace.is_empty();
            for n in namespaces {
                if has_namespace {
                    namespace += "::";
                } else {
                    has_namespace = true;
                }
                namespace += n;
            }
        }
        if self.function_type == FunctionWriteMode::Inline && config.language == Language::Cxx &&
           config.structure.generate_member_functions {
            if !namespace.is_empty() {
                namespace += "::";
            }
            namespace += "intern";
        }
        namespace += "::";
        namespace
    }

    pub fn write_function_call<W: Write>(&self,
                                         config: &Config,
                                         out: &mut SourceWriter<W>,
                                         args: &[String],
                                         layout_vertical: bool) {
        let del = if layout_vertical { "," } else { ", " };
        let namespace = self.get_namespace(config);
        if self.ret == Type::Primitive(PrimitiveType::Void) {
            out.write(&namespace);
        } else {
            out.write(&format!("return {}", namespace));
        }
        out.write(&self.name);
        out.write("(");
        let align_length = out.line_length_for_align();
        out.push_set_spaces(align_length);
        let mut first = true;
        for arg in args {
            if first {
                first = false;
            } else {
                out.write(del);
            }
            if layout_vertical {
                out.new_line();
            }
            out.write(arg);
        }
        out.pop_tab();
        out.write(");");
    }
}

impl Source for Function {
    fn write<F: Write>(&self, config: &Config, out: &mut SourceWriter<F>) {
        if self.extern_decl {
            return;
        }
        if self.function_type == FunctionWriteMode::Intern && config.language == Language::Cxx &&
           config.structure.generate_member_functions {
            out.write("namespace intern");
            out.open_brace();
        }
        self.documentation.write(config, out);

        let option_1 = out.measure(|out| self.write_function_header(config, out, false));

        if (config.function.args == Layout::Auto && option_1 <= config.line_length) ||
           config.function.args == Layout::Horizontal {
            self.write_function_header(config, out, false);
        } else {
            self.write_function_header(config, out, true);
        }
        let args = match self.function_type {
            FunctionWriteMode::Global => return,
            FunctionWriteMode::Intern => {
                if config.language == Language::Cxx && config.structure.generate_member_functions {
                    out.close_brace(false);
                }
                return;
            }
            FunctionWriteMode::MemberFunction => {
                [String::from("this")]
                    .iter()
                    .cloned()
                    .chain(self.args[1..].iter().map(|&(ref name, _)| name.clone()))
                    .collect::<Vec<_>>()
            }
            FunctionWriteMode::Inline => {
                self.args
                    .iter()
                    .map(|&(ref name, _)| name.clone())
                    .collect()
            }
        };
        out.open_brace();
        let option_1 = out.measure(|out| self.write_function_call(config, out, &args, false));

        if (config.function.args == Layout::Auto && option_1 <= config.line_length) ||
           config.function.args == Layout::Horizontal {
            self.write_function_call(config, out, &args, false);
        } else {
            self.write_function_call(config, out, &args, true);
        }
        out.close_brace(false);
    }
}

pub trait SynFnArgHelpers {
    fn as_ident_and_type(&self) -> Result<Option<(String, Type)>, String>;
}

impl SynFnArgHelpers for syn::FnArg {
    fn as_ident_and_type(&self) -> Result<Option<(String, Type)>, String> {
        match self {
            &syn::FnArg::Typed(ref tpe) => {
                if let Some(x) = Type::load(&tpe.ty)? {
                    match &*tpe.pat {
                        syn::Pat::Path(path) => {
                            if let Some(ident) = path.path.get_ident() {
                                Ok(Some((ident.to_string(), x)))
                            } else {
                                Err(format!("Invalid path: {:?}", path.path))
                            }
                        }
                        syn::Pat::Ident(ident) => {
                            Ok(Some((ident.ident.to_string(), x)))
                        }
                        _ => Err(format!("Unexpected paramater format for {:?}", tpe)),
                    }
                } else {
                    Ok(None)
                }
            }
            _ => Err(format!("parameter has unexpected type")),
        }
    }
}
