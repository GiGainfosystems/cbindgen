/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

use std::io::Write;
use std::fmt::{self, Display};

use bindgen::dependency_graph::{Item, DependencyKind};
use bindgen::config::{Config, Language, Layout};
use bindgen::ir::*;
use bindgen::rename::*;
use bindgen::writer::*;

#[derive(Debug, Clone)]
pub struct Class {
    pub name: String,
    pub inner: Struct,
    pub functions: Vec<Function>,
    pub destructor: Option<Function>,
    pub specialization: Option<Specialization>,
}

impl Display for Class {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Class {}", self.name)
    }
}

impl Class {
    pub fn from(mut inner: Struct, members: Vec<Function>) -> Self {
        let name = inner.name.clone();
        inner.name = format!("{}_intern", name);
        inner.intern = true;
        let mut ret = Class {
            name,
            inner,
            functions: Vec::new(),
            destructor: None,
            specialization: None,
        };
        ret.add_member_functions(members);
        ret.specialization = ::std::mem::replace(&mut ret.inner.specialization, None);
        ret
    }

    pub fn as_opaque(&self) -> OpaqueItem {
        OpaqueItem {
            name: self.name.clone(),
            generic_params: self.inner.generic_params.clone(),
            annotations: self.inner.annotations.clone(),
            documentation: self.inner.documentation.clone(),
        }
    }

    pub fn get_deps(&self) -> Vec<(Item, DependencyKind)> {
        let mut ret = vec![(Item::Struct(self.inner.clone()), DependencyKind::Normal)];
        if let Some(ref s) = self.specialization {
            ret.push((Item::Specialization(s.clone()), DependencyKind::Normal));
        }
        for f in &self.functions {
            let mut f = f.clone();
            f.function_type = FunctionWriteMode::Global;
            ret.push((Item::Function(f), DependencyKind::Ptr));
        }
        if let Some(ref d) = self.destructor {
            ret.push((Item::Function(d.clone()), DependencyKind::Ptr));
        }
        ret
    }

    pub fn rename_types(&mut self, config: &Config) {
        for f in &mut self.functions {
            f.rename_args(config);
        }
        if let Some(ref mut destructor) = self.destructor {
            destructor.rename_args(config);
        }
    }

    pub fn mangle_paths(&mut self) {
        for f in &mut self.functions {
            f.mangle_paths();
        }
        if let Some(ref mut destructor) = self.destructor {
            destructor.mangle_paths();
        }
    }

    fn add_member_functions(&mut self, functions: Vec<Function>) {
        for mut function in functions {
            if function.annotations.bool("destructor").unwrap_or(false) &&
               self.destructor.is_none() && function.args.len() == 1 &&
               function.ret == Type::Primitive(PrimitiveType::Void) {
                self.destructor = Some(function);
            } else if !function.annotations.bool("destructor").unwrap_or(false) &&
                      !function.annotations.bool("skip-member").unwrap_or(false) {
                function.function_type = FunctionWriteMode::MemberFunction;
                self.functions.push(function);
            } else if function.annotations.bool("destructor").unwrap_or(false) {
                warn!("Found double destructor annotation for struct {}",
                      self.name);
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
                let option_1 = out.measure(|out| {
                    destructor.write_function_call(config, out, &[String::from("this")], false)
                });

                if (config.function.args == Layout::Auto && option_1 <= config.line_length) ||
                   config.function.args == Layout::Horizontal {
                    destructor.write_function_call(config, out, &[String::from("this")], false)
                } else {
                    destructor.write_function_call(config, out, &[String::from("this")], true)
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
            f.write(config, out);
            out.new_line();
        }
    }
}

impl Source for Class {
    fn write<W: Write>(&self, config: &Config, out: &mut SourceWriter<W>) {
        if config.language == Language::Cxx {
            out.write(&format!("struct {}: public intern::{}", self.name, self.inner.name));
            out.open_brace();
            out.write(&format!("{}(intern::{}&& other)", self.name, self.inner.name));
            out.open_brace();
            for f in &self.inner.fields {
                out.write(&format!("this->{0} = other.{0};", f.name));
                out.new_line();
            }
            out.close_brace(false);

            self.write_destructor(config, out);
            let mut wrote_start_newline = false;

            let other = if let Some(r) = config.function.rename_args {
                r.apply_to_snake_case("other", IdentifierType::FunctionArg)
            } else {
                String::from("other")
            };

            {
                let mut emit_op = |op, conjuc| {
                    if !wrote_start_newline {
                        wrote_start_newline = true;
                        out.new_line();
                    }

                    out.new_line();

                    out.write(&format!("bool operator{}(const {}& {}) const",
                                      op,
                                      self.name,
                                      other));
                    out.open_brace();
                    out.write("return ");
                    out.write_vertical_list(&self.inner
                                                 .fields
                                                 .iter()
                                                 .map(|x| {
                                                          format!("{0} {1} {2}.{0}",
                                                                  x.name,
                                                                  op,
                                                                  other)
                                                      })
                                                 .collect(),
                                            ListType::Join(&format!(" {}", conjuc)));
                    out.write(";");
                    out.close_brace(false);
                };

                if config.structure.derive_eq(&self.inner.annotations) &&
                   !self.inner.fields.is_empty() &&
                   self.inner.fields.iter().all(|x| x.tpe.can_cmp_eq()) {
                    emit_op("==", "&&");
                }
                if config.structure.derive_neq(&self.inner.annotations) &&
                   !self.inner.fields.is_empty() &&
                   self.inner.fields.iter().all(|x| x.tpe.can_cmp_eq()) {
                    emit_op("!=", "||");
                }
                if config.structure.derive_lt(&self.inner.annotations) &&
                   self.inner.fields.len() == 1 &&
                   self.inner.fields[0].tpe.can_cmp_order() {
                    emit_op("<", "&&");
                }
                if config.structure.derive_lte(&self.inner.annotations) &&
                   self.inner.fields.len() == 1 &&
                   self.inner.fields[0].tpe.can_cmp_order() {
                    emit_op("<=", "&&");
                }
                if config.structure.derive_gt(&self.inner.annotations) &&
                   self.inner.fields.len() == 1 &&
                   self.inner.fields[0].tpe.can_cmp_order() {
                    emit_op(">", "&&");
                }
                if config.structure.derive_gte(&self.inner.annotations) &&
                   self.inner.fields.len() == 1 &&
                   self.inner.fields[0].tpe.can_cmp_order() {
                    emit_op(">=", "&&");
                }
            }
            self.write_functions(config, out);
            out.close_brace(true);
        }
    }
}
