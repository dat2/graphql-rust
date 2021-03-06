use std::collections::HashMap;

pub type Name = String;

// ===========================================================================
// 2.1 Document
// ===========================================================================
#[derive(Debug,PartialEq,Clone)]
pub struct Document {
  definitions: Vec<Definition>,
}

impl Document {
  pub fn new(definitions: Vec<Definition>) -> Document {
    Document { definitions: definitions }
  }
}

#[derive(Debug,PartialEq,Clone)]
pub enum Definition {
  Operation(Operation),
  Fragment(Fragment),
}

// ===========================================================================
// 2.2 Operations
// ===========================================================================
#[derive(Debug,PartialEq,Clone)]
pub struct Operation {
  op_type: OperationType,
  name: Option<Name>,
  variable_definitions: Vec<VariableDefinition>,
  directives: Vec<Directive>,
  selection_set: Vec<Selection>,
}

impl Operation {
  pub fn new(op_type: OperationType,
             name: Option<Name>,
             variable_definitions: Vec<VariableDefinition>,
             directives: Vec<Directive>,
             selection_set: Vec<Selection>)
             -> Operation {
    Operation {
      op_type: op_type,
      name: name,
      variable_definitions: variable_definitions,
      directives: directives,
      selection_set: selection_set,
    }
  }
}

#[derive(Debug,PartialEq,Clone)]
pub enum OperationType {
  Query,
  Mutation,
}

// ===========================================================================
// 2.4 Selection Sets
// ===========================================================================

#[derive(Debug,PartialEq,Clone)]
pub enum Selection {
  Field(Field),
  FragmentSpread(FragmentSpread),
  InlineFragment(InlineFragment),
}

// ===========================================================================
// 2.5 Fields
// ===========================================================================

#[derive(Debug,PartialEq,Clone)]
pub struct Field {
  alias: Option<Name>,
  name: Name,
  arguments: Vec<Argument>,
  directives: Vec<Directive>,
  selection_set: Vec<Selection>,
}

impl Field {
  pub fn new(alias: Option<Name>,
             name: Name,
             arguments: Vec<Argument>,
             directives: Vec<Directive>,
             selection_set: Vec<Selection>)
             -> Field {
    Field {
      alias: alias,
      name: name,
      arguments: arguments,
      directives: directives,
      selection_set: selection_set,
    }
  }
}

// ===========================================================================
// 2.6 Arguments
// ===========================================================================

#[derive(Debug,PartialEq,Clone)]
pub struct Argument {
  name: Name,
  value: Value,
}

impl Argument {
  pub fn new(name: Name, value: Value) -> Argument {
    Argument {
      name: name,
      value: value,
    }
  }
}

// ===========================================================================
// 2.7 Field Alias
// ===========================================================================

// ===========================================================================
// 2.8 Fragments
// ===========================================================================
#[derive(Debug,PartialEq,Clone)]
pub struct FragmentSpread {
  name: Name,
  directives: Vec<Directive>,
}

impl FragmentSpread {
  pub fn new(name: Name, directives: Vec<Directive>) -> FragmentSpread {
    FragmentSpread {
      name: name,
      directives: directives,
    }
  }
}

#[derive(Debug,PartialEq,Clone)]
pub struct InlineFragment {
  type_condition: Option<Type>,
  directives: Vec<Directive>,
  selection_set: Vec<Selection>,
}

impl InlineFragment {
  pub fn new(type_condition: Option<Type>,
             directives: Vec<Directive>,
             selection_set: Vec<Selection>)
             -> InlineFragment {
    InlineFragment {
      type_condition: type_condition,
      directives: directives,
      selection_set: selection_set,
    }
  }
}

#[derive(Debug,PartialEq,Clone)]
pub struct Fragment {
  name: Name,
  type_condition: Type,
  directives: Vec<Directive>,
  selection_set: Vec<Selection>,
}

impl Fragment {
  pub fn new(name: Name, type_condition: Type, directives: Vec<Directive>, selection_set: Vec<Selection>) -> Fragment {
    Fragment {
      name: name,
      type_condition: type_condition,
      directives: directives,
      selection_set: selection_set,
    }
  }
}

// ===========================================================================
// 2.9 Values
// ===========================================================================
#[derive(Debug,PartialEq,Clone)]
pub enum Value {
  Variable(Variable),
  Int(i32),
  Float(f32),
  String(String),
  Boolean(bool),
  Null,
  Enum(String),
  List(Vec<Value>),
  Object(HashMap<String, Value>),
}

pub type Variable = String;

// ===========================================================================
// 2.10 Variables
// ===========================================================================
#[derive(Debug,PartialEq,Clone)]
pub struct VariableDefinition {
  variable: Variable,
  var_type: Type,
  default_value: Option<Value>,
}

impl VariableDefinition {
  pub fn new(variable: Variable, var_type: Type, default_value: Option<Value>) -> VariableDefinition {
    VariableDefinition {
      variable: variable,
      var_type: var_type,
      default_value: default_value,
    }
  }
}

// ===========================================================================
// 2.11 Types
// ===========================================================================
#[derive(Debug,PartialEq,Clone)]
pub enum Type {
  Named(Name),
  List(Box<Type>),
  NonNull(Box<Type>),
}

// ===========================================================================
// 2.12 Directives
// ===========================================================================

#[derive(Debug,PartialEq,Clone)]
pub struct Directive {
  name: Name,
  arguments: Vec<Argument>,
}

impl Directive {
  pub fn new(name: Name, arguments: Vec<Argument>) -> Directive {
    Directive {
      name: name,
      arguments: arguments,
    }
  }
}
