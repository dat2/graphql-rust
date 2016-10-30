use std::collections::HashMap;

use chomp::types::{Buffer, U8Input, ParseResult};
use chomp::ascii::{is_alpha, is_alphanumeric};
use chomp::parsers::{SimpleResult, Error, any, token, satisfy, string, take_while, skip_while};
use chomp::combinators::{many_till, many, option, either, or};

pub type Name = String;
pub type SelectionSet = Vec<Selection>;

#[derive(Debug,PartialEq)]
pub struct Document {
  definitions: Vec<Definition>,
}

#[derive(Debug,PartialEq)]
pub enum Definition {
  OperationDefinition(Operation),
  SelectionSetDefinition(SelectionSet),
  FragmentDefinition(Fragment),
}

#[derive(Debug,PartialEq)]
pub struct Operation {
  op_type: OperationType,
  name: Option<Name>,
  variable_definitions: Vec<VariableDefinition>,
  directives: Vec<Directive>,
}

impl Operation {
  fn new(op_type: OperationType, name: Option<Name>, variable_definitions: Vec<VariableDefinition>, directives: Vec<Directive>) -> Operation {
    Operation { op_type: op_type, name: name, variable_definitions: variable_definitions, directives: directives }
  }
}

#[derive(Debug,PartialEq)]
pub enum OperationType {
  Query,
  Mutation,
}

#[derive(Debug,PartialEq)]
pub struct VariableDefinition {
  variable: Variable,
  var_type: Type,
  default_value: Option<Value>,
}

type Variable = String;

#[derive(Debug,PartialEq)]
pub enum Type {
  NamedType(Name),
  ListType(Box<Type>),
  NonNullType(Box<Type>),
}

#[derive(Debug,PartialEq)]
pub enum Value {
  Var(Variable),
  IntValue(i32),
  FloatValue(f32),
  StringValue(String),
  BooleanValue(bool),
  NullValue,
  EnumValue(String),
  ListValue(Vec<Value>),
  ObjectValue(HashMap<String, Value>),
}

#[derive(Debug,PartialEq)]
pub struct Directive {
  name: Name,
  arguments: Vec<Argument>,
}

#[derive(Debug,PartialEq)]
pub struct Argument {
  name: Name,
  value: Value,
}

#[derive(Debug,PartialEq)]
pub enum Selection {
  FieldSelection(Field),
  FragmentSpread(Name, Vec<Directive>),
  InlineFragment(Option<Type>, Vec<Directive>, SelectionSet),
}

#[derive(Debug,PartialEq)]
pub struct Field {
  alias: Option<Name>,
  name: Name,
  arguments: Vec<Argument>,
  directives: Vec<Directive>,
  selection_set: SelectionSet,
}

impl Field {
  fn new(alias: Option<Name>, name: Name, arguments: Vec<Argument>, directives: Vec<Directive>, selection_set: SelectionSet) -> Field {
    Field { alias: alias, name: name, arguments: arguments, directives: directives, selection_set: selection_set }
  }
}

#[derive(Debug,PartialEq)]
pub struct Fragment {
  name: Name,
  type_condition: Type,
  directives: Vec<Directive>,
  selection_set: SelectionSet,
}

pub fn white_space<I: U8Input>(i: I) -> SimpleResult<I, ()>
{
  skip_while(i, |c| c == b' ' || c == b'\t')
}

pub fn line_terminator<I: U8Input>(i: I) -> SimpleResult<I, ()>
{
  parse!{i;
    string(b"\r\n") <|> string(b"\r") <|> string(b"\n") >> ret ()
  }
}

pub fn comment<I: U8Input>(i: I) -> SimpleResult<I, ()>
{
  parse!{i;
    token(b'#');
    let _rest: Vec<_> = many_till(any, line_terminator);
    ret ()
  }
}

pub fn comma<I: U8Input>(i: I) -> SimpleResult<I,u8>
{
  token(i, b',')
}

pub fn operation_definition<I: U8Input>(i: I) -> SimpleResult<I,Definition>
{
  parse!{i;
    let op_type = operation_type();
    white_space() <|> line_terminator();
    let name = option(|i| name(i).map(Some), None);

    ret {
      let op = Operation::new(op_type, name, Vec::new(), Vec::new());
      Definition::OperationDefinition(op)
    }
  }
}

pub fn operation_type<I: U8Input>(i: I) -> SimpleResult<I,OperationType>
{
  let op_type = |i,b,r| string(i,b).map(|_| r);
  parse!{i;
      op_type(b"query", OperationType::Query) <|>
      op_type(b"mutation", OperationType::Mutation)
  }
}

pub fn name<I: U8Input>(i: I) -> SimpleResult<I,Name>
{
  parse!{i;
    let start = satisfy(|c| is_alpha(c) || c == b'_');
    let rest = take_while(|c| is_alphanumeric(c) || c == b'_');

    ret {
      let mut start = String::from_utf8(vec![start]).unwrap();
      let rest = String::from_utf8(rest.to_vec()).unwrap();
      start.push_str(&rest);
      start
    }
  }
}

pub fn alias<I: U8Input>(i: I) -> SimpleResult<I, Option<Name>>
{
  let parser = parse!{i;
    let name = name();

    either(white_space, line_terminator);
    token(b':');

    ret Some(name)
  };

  option(i, parser, None)
}

#[cfg(test)]
mod tests {
  // use super::{OperationType, Operation, Definition white_space, line_terminator, comment, comma, operation_type, name, operation_definition};
  use super::*;
  use chomp::prelude::parse_only;

  #[test]
  fn test_parse_comment() {
    assert_eq!(parse_only(comment, b"#test\r"), Ok(()));
    assert_eq!(parse_only(comment, b"#test\n"), Ok(()));
    assert_eq!(parse_only(comment, b"#test\r\n"), Ok(()));
  }

  #[test]
  fn test_operation_type() {
    assert_eq!(parse_only(operation_type, b"query"), Ok(OperationType::Query));
    assert_eq!(parse_only(operation_type, b"mutation"), Ok(OperationType::Mutation));
  }
}
