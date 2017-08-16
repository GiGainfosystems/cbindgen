/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */
use std::hash::{Hash, Hasher};
use std::fmt::{self, Display};
use std::collections::{HashMap, HashSet};
use petgraph::{Graph, Direction};
use petgraph::graph::{NodeIndex, EdgeIndex};
use bindgen::ir::*;
use bindgen::library::Library;
use bindgen::writer::*;
use bindgen::mangle;
use bindgen::config::{Config, Language};
use std::io::Write;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum DependencyKind {
    Ptr,
    Normal,
}

impl Display for DependencyKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            DependencyKind::Ptr => write!(f, "Ptr"),
            DependencyKind::Normal => write!(f, "Normal"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Item {
    Enum(Enum),
    Struct(Struct),
    Object(Class),
    Opaque(OpaqueItem),
    Typedef(Typedef),
    Function(Function),
    WrappedFunction(Function),
    Specialization(Specialization),
}

impl Item {
    fn name(&self) -> &str {
        match *self {
            Item::Enum(ref e) => &e.name,
            Item::Struct(ref s) => &s.name,
            Item::Opaque(ref o) => &o.name,
            Item::Typedef(ref t) => &t.name,
            Item::WrappedFunction(ref f) |
            Item::Function(ref f) => &f.name,
            Item::Specialization(ref s) => &s.name,
            Item::Object(ref o) => &o.name,
        }
    }

    fn get_deps(&self, library: &Library) -> Vec<(Item, DependencyKind)> {
        match *self {
            Item::Enum(_) | Item::Opaque(_) => Vec::new(),
            Item::Specialization(ref s) => s.get_deps(library),
            Item::Struct(ref s) => s.get_deps(library),
            Item::Typedef(ref t) => t.get_deps(library),
            Item::Function(ref f) => f.get_deps(library),
            Item::Object(ref o) => o.get_deps(),
            Item::WrappedFunction(ref f) => {
                vec![(Item::Function(f.clone()), DependencyKind::Normal)]
            }
        }
    }

    fn mangle_paths(&mut self) {
        match *self {
            Item::Enum(_) |
            Item::Opaque(_) |
            Item::Specialization(_) => {}
            Item::Struct(ref mut s) => s.mangle_paths(),
            Item::Typedef(ref mut t) => t.mangle_paths(),
            Item::WrappedFunction(ref mut f) |
            Item::Function(ref mut f) => f.mangle_paths(),
            Item::Object(ref mut o) => o.mangle_paths(),

        }
    }

    fn apply_transformation(&mut self, lib: &Library) {
        match *self {
            Item::Enum(ref mut e) => {
                e.rename_fields(&lib.config);
            }
            Item::Struct(ref mut s) => {
                s.rename_fields(&lib.config);
            }
            Item::WrappedFunction(ref mut f) |
            Item::Function(ref mut f) => {
                f.rename_args(&lib.config);
            }
            Item::Object(ref mut o) => {
                o.rename_types(&lib.config);
            }
            _ => {}
        }
    }
}

impl Display for Item {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Item::Enum(ref e) => write!(f, "Enum {}", e.name),
            Item::Struct(ref s) => write!(f, "Struct {}", s.name),
            Item::Opaque(ref o) => write!(f, "Opaque {}", o.name),
            Item::Typedef(ref t) => write!(f, "Typedef {}", t.name),
            Item::WrappedFunction(ref c) => write!(f, "InlineFunction {}", c.name),
            Item::Function(ref c) => write!(f, "Function {}", c.name),
            Item::Object(ref o) => write!(f, "Class {}", o.name),
            Item::Specialization(ref s) if !s.generic_values.is_empty() => {
                write!(f, "Specialization {}<", s.name)?;
                let mut first = true;
                for g in &s.generic_values {
                    if first {
                        first = false;
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, "{:?}", g.get_root_path())?;
                }
                write!(f, ">")
            }
            Item::Specialization(ref s) => {
                write!(f, "Specialization {}<", s.name)?;
                let mut first = true;
                for g in &s.generic_params {
                    if first {
                        first = false;
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", g)?;
                }
                write!(f, ">")
            }
        }
    }
}

impl Hash for Item {
    fn hash<H>(&self, state: &mut H)
        where H: Hasher
    {
        self.name().hash(state);
        match *self {
            Item::Enum(_) => "enum".hash(state),
            Item::Struct(_) => "struct".hash(state),
            Item::Opaque(_) => "opaque".hash(state),
            Item::Typedef(_) => "typedef".hash(state),
            Item::WrappedFunction(_) => "wrapppedfunction".hash(state),
            Item::Function(_) => "function".hash(state),
            Item::Object(_) => "object".hash(state),
            Item::Specialization(ref s) => {
                "specialization".hash(state);
                s.generic_values.hash(state);
            }
        }
    }
}

impl PartialEq<Self> for Item {
    fn eq(&self, rhs: &Self) -> bool {
        match (self, rhs) {
            (&Item::Enum(ref e1), &Item::Enum(ref e2)) => e1.name == e2.name,
            (&Item::Struct(ref s1), &Item::Struct(ref s2)) => s1.name == s2.name,
            (&Item::Opaque(ref o1), &Item::Opaque(ref o2)) => o1.name == o2.name,
            (&Item::Typedef(ref t1), &Item::Typedef(ref t2)) => t1.name == t2.name,
            (&Item::WrappedFunction(ref f1), &Item::WrappedFunction(ref f2)) |
            (&Item::Function(ref f1), &Item::Function(ref f2)) => f1.name == f2.name,
            (&Item::Object(ref o1), &Item::Object(ref o2)) => o1.name == o2.name,
            (&Item::Specialization(ref s1), &Item::Specialization(ref s2)) => {
                s1.name == s2.name && s1.generic_values == s2.generic_values
            }
            _ => false,
        }
    }
}

impl Eq for Item {}

impl Source for Item {
    fn write<F: Write>(&self, config: &Config, out: &mut SourceWriter<F>) {
        match *self {
            Item::Enum(ref e) => e.write(config, out),
            Item::Struct(ref s) => s.write(config, out),
            Item::Opaque(ref o) => o.write(config, out),
            Item::Typedef(ref t) => t.write(config, out),
            Item::WrappedFunction(ref f) => f.write(config, out),
            Item::Function(ref f) => f.write(config, out),
            Item::Specialization(ref s) => s.write(config, out),
            Item::Object(ref o) => o.write(config, out),
        }
    }
}

/// A dependency list is used for gathering what order to output the types.
pub struct DependencyList {
    graph: Graph<Item, DependencyKind>,
    lookup: HashMap<Item, NodeIndex>,
    classes: HashSet<String>,
}

impl DependencyList {
    pub fn new(functions: &[Function], library: &Library) -> Self {
        let mut d = DependencyList {
            graph: Graph::new(),
            lookup: HashMap::new(),
            classes: HashSet::new(),
        };
        for f in functions {
            d.add_dep(Item::Function(f.clone()), library);
        }
        d
    }

    fn type_name(ty: &Type) -> Option<String> {
        match *ty {
            Type::Ptr(ref t) |
            Type::ConstPtr(ref t) => {
                match **t {
                    Type::Path(ref p, ref g) => Some(mangle::mangle_path(p, g)),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    fn add_dep(&mut self, mut item: Item, library: &Library) {
        if !self.lookup.contains_key(&item) {
            item.apply_transformation(library);
            let idx = self.graph.add_node(item.clone());
            self.lookup.insert(item.clone(), idx);
            let mut deps = item.get_deps(library);
            if library.config.structure.generate_member_functions &&
               library.config.language == Language::Cxx {
                deps = deps.into_iter()
                    .map(|(dep, kind)| if let Item::Struct(d) = dep {

                             let members = library
                                 .functions
                                 .iter()
                                 .filter(|f| if let Some(arg) = f.args.get(0) {
                                             Self::type_name(&arg.1)
                                                 .map(|n| n == d.name)
                                                 .unwrap_or(false)
                                         } else {
                                             false
                                         })
                                 .cloned()
                                 .collect::<Vec<_>>();
                             if members.is_empty() {
                                 (Item::Struct(d), kind)
                             } else {
                                 self.classes.insert(d.name.clone());
                                 (Item::Object(Class::from(d, members)), kind)
                             }
                         } else {
                             (dep, kind)
                         })
                    .collect();
            }
            for &(ref d, _) in &deps {
                self.add_dep(d.clone(), library);
            }
            for (d, k) in deps {
                if let Some(to_id) = self.lookup.get(&d) {
                    match d {
                        Item::Specialization(ref s) if !s.generic_values.is_empty() => {
                            self.graph.add_edge(*to_id, idx, k);
                        }
                        _ => {
                            self.graph.add_edge(idx, *to_id, k);
                        }
                    }
                } else {
                    println!("Did not found {:?}", d);
                    panic!();
                }
            }
        }
    }

    pub fn fix_abi(&mut self) {
        use petgraph::visit::EdgeRef;
        let nodes = self.graph.node_indices().collect::<Vec<_>>();
        for nid in nodes {
            let function = match self.graph.node_weight_mut(nid) {
                Some(&mut Item::Function(ref mut f)) => {
                    let mut ret = f.clone();
                    match f.ret {
                        Type::Path(ref mut p, ref mut g) => {
                            let name = mangle::mangle_path(p, g);
                            if !self.classes.contains(&name) {
                                continue;
                            }

                            *p = format!("{}_intern", name);
                            *g = Vec::new();
                        }
                        _ => continue,
                    }
                    f.function_type = FunctionWriteMode::Intern;
                    ret.function_type = FunctionWriteMode::Inline;
                    Item::WrappedFunction(ret)
                }
                _ => continue,
            };
            let DependencyList {
                ref mut lookup,
                ref mut graph,
                ..
            } = *self;
            let wrapped_id = lookup
                .entry(function.clone())
                .or_insert_with(|| {
                                    let id = graph.add_node(function);
                                    id
                                });
            loop {
                let (id, from, to) =
                    if let Some(e) = graph.edges_directed(nid, Direction::Incoming).next() {
                        if e.source() == nid {
                            (e.id(), *wrapped_id, e.target())
                        } else {
                            (e.id(), e.source(), *wrapped_id)
                        }
                    } else {
                        break;
                    };
                if let Some(kind) = graph.remove_edge(id) {
                    graph.add_edge(from, to, kind);
                }
            }
            graph.add_edge(*wrapped_id, nid, DependencyKind::Normal);
        }
    }

    // It's there for debugging
    #[allow(dead_code)]
    pub fn print(&self) {
        use petgraph::dot::Dot;
        println!("{}", Dot::new(&self.graph));
    }

    fn generate_opaque_item(&self,
                            id: NodeIndex,
                            o: OpaqueItem,
                            ret: &mut Vec<Item>)
                            -> Option<Vec<EdgeIndex>> {
        use petgraph::visit::EdgeRef;
        // It is possible to have multiple edges with different
        // dependencies between nodes, so we need to group the edges by
        // theire source
        let mut edges = HashMap::new();
        for e in self.graph.edges_directed(id, Direction::Incoming) {
            edges.entry(e.source()).or_insert_with(Vec::new).push(e);
        }
        // We would only remove edges with a ptr dependency between nodes
        // by injecting a opaque wrapper
        let edges = edges
            .values()
            .filter(|edges| edges.iter().all(|e| e.weight() == &DependencyKind::Ptr))
            .flat_map(|edges| edges.iter().map(|e| e.id()))
            .collect::<Vec<_>>();
        // If there is node ptr dependency we are done here
        if edges.is_empty() {
            None
        } else {
            ret.push(Item::Opaque(o));
            Some(edges)
        }
    }

    fn remove_cycle(&mut self, id: NodeIndex, ret: &mut Vec<Item>) {
        let edges = {
            let node = self.graph.node_weight(id).expect("Got id from graph above");
            match *node {
                Item::Struct(ref s) => self.generate_opaque_item(id, s.as_opaque(), ret),
                Item::Object(ref o) => self.generate_opaque_item(id, o.as_opaque(), ret),
                _ => return,
            }
        };
        if let Some(edges) = edges {
            for e in edges {
                self.graph.remove_edge(e);
            }
        }
    }

    pub fn calculate_order(mut self) -> Vec<Item> {
        let mut ret = Vec::new();
        let mut cycle_counter = 0;
        while self.graph.node_count() > 0 {
            // find structs without any dependency
            let externals = self.graph
                .externals(Direction::Outgoing)
                .collect::<Vec<_>>();
            if externals.is_empty() {
                if cycle_counter >= self.graph.node_count() {
                    self.print();
                    panic!("Could not remove cycle");
                }
                // there is a cyclic graph left, so we add a struct as opaque
                // item and remove some edge from the dependceny graph
                let id = self.graph
                    .node_indices()
                    .skip(cycle_counter)
                    .next()
                    .expect("Graph is not empty");
                self.remove_cycle(id, &mut ret);
                cycle_counter += 1;
            } else {
                cycle_counter = 0;
                // Iterate over all nodes without dependency
                // 1. Remove them from the graph
                // 2. Push them to the orderd struct list
                for idx in externals {
                    if let Some(mut s) = self.graph.remove_node(idx) {
                        s.mangle_paths();
                        ret.push(s);
                    }
                }
            }
        }
        ret
    }
}
