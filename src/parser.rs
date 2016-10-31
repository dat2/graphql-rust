// #![deny(missing_docs)]

use std::collections::HashMap;

use std::marker::PhantomData;

use combine::{Parser, ParseResult, Stream};
use combine::char::{tab, char, crlf, string, letter, alpha_num};
use combine::combinator::{between, many, none_of, or, optional, value};

pub type Name = String;
pub type SelectionSet = Vec<Selection>;

#[derive(Debug,PartialEq,Clone)]
pub struct Document {
  definitions: Vec<Definition>,
}

#[derive(Debug,PartialEq,Clone)]
pub enum Definition {
  Operation(Operation),
  SelectionSet(SelectionSet),
  Fragment(Fragment),
}

#[derive(Debug,PartialEq,Clone)]
pub struct Operation {
  op_type: OperationType,
  name: Option<Name>,
  variable_definitions: Vec<VariableDefinition>,
  directives: Vec<Directive>,
}

impl Operation {
  fn new(op_type: OperationType,
         name: Option<Name>,
         variable_definitions: Vec<VariableDefinition>,
         directives: Vec<Directive>)
         -> Operation {
    Operation {
      op_type: op_type,
      name: name,
      variable_definitions: variable_definitions,
      directives: directives,
    }
  }
}

#[derive(Debug,PartialEq,Clone)]
pub enum OperationType {
  Query,
  Mutation,
}

#[derive(Debug,PartialEq,Clone)]
pub struct VariableDefinition {
  variable: Variable,
  var_type: Type,
  default_value: Option<Value>,
}

impl VariableDefinition {
  fn new(variable: Variable, var_type: Type, default_value: Option<Value>) -> VariableDefinition {
    VariableDefinition {
      variable: variable,
      var_type: var_type,
      default_value: default_value,
    }
  }
}

pub type Variable = String;

#[derive(Debug,PartialEq,Clone)]
pub enum Type {
  Named(Name),
  List(Box<Type>),
  NonNull(Box<Type>),
}

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

#[derive(Debug,PartialEq,Clone)]
pub struct Directive {
  name: Name,
  arguments: Vec<Argument>,
}

#[derive(Debug,PartialEq,Clone)]
pub struct Argument {
  name: Name,
  value: Value,
}

#[derive(Debug,PartialEq,Clone)]
pub enum Selection {
  Field(Field),
  FragmentSpread(Name, Vec<Directive>),
  InlineFragment(Option<Type>, Vec<Directive>, SelectionSet),
}

#[derive(Debug,PartialEq,Clone)]
pub struct Field {
  alias: Option<Name>,
  name: Name,
  arguments: Vec<Argument>,
  directives: Vec<Directive>,
  selection_set: SelectionSet,
}

impl Field {
  fn new(alias: Option<Name>,
         name: Name,
         arguments: Vec<Argument>,
         directives: Vec<Directive>,
         selection_set: SelectionSet)
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

#[derive(Debug,PartialEq,Clone)]
pub struct Fragment {
  name: Name,
  type_condition: Type,
  directives: Vec<Directive>,
  selection_set: SelectionSet,
}

macro_rules! make_parser {

    // base case
    () => {};

    ($name:ident ($input_var:ident : $input_item_type:ty) -> $output_type:ty { $($tmpl:tt)* } $($rest:tt)*) => {

      #[derive(Clone)]
      #[allow(missing_docs)]
      pub struct $name<T: Clone> {
        _phantom: PhantomData<T>,
      }

      impl<T: Clone> $name<T> {
          #[allow(missing_docs)]
          pub fn new() -> Self {
              $name {
                _phantom: PhantomData
              }
          }
      }

      impl<I> Parser for $name<I> where I: Stream<Item=$input_item_type> {
        type Input = I;
        type Output = $output_type;

        fn parse_stream(&mut self, $input_var: I) -> ParseResult<Self::Output, Self::Input> {
          $($tmpl)*
        }
      }

      make_parser!($($rest)*);
    };

    ($name:ident ($input_var:ident : $input_item_type:ty , $($field:ident : &$typ:ty),*)
      -> $output_type:ty { $($tmpl:tt)* } $($rest:tt)*) => {

        #[derive(Clone)]
        #[allow(missing_docs)]
        pub struct $name<'a, T: Clone> {
          _phantom: PhantomData<T>,
          $( $field: &'a $typ),*
        }

        impl<'a, T: Clone> $name<'a, T> {
          #[allow(missing_docs)]
          pub fn new($($field: &'a $typ),*) -> Self {
            $name {
              _phantom: PhantomData,
              $( $field: $field),*
            }
          }
        }

        impl<'a, I> Parser for $name<'a, I> where I: Stream<Item=$input_item_type> {
          type Input = I;
          type Output = $output_type;

          fn parse_stream(&mut self, $input_var: I) -> ParseResult<Self::Output, Self::Input> {
            let &mut $name { _phantom, $($field),* } = self;

            $($tmpl)*
          }
        }

        make_parser!($($rest)*);
    };
}

// TODO graphql and char have a very differing set of code points
// graphql :: [0009,000A,000D, [0020,FFFF] ]
// char :: [0,D7FF] u [E000,10FFFF]
// somehow we need to wrangle char to match the graphql types :)

make_parser!(
  WhiteSpace(input: char) -> char {
    char(' ').or(tab())
      .parse_stream(input)
  }
);

make_parser!(
  LineTerminator(input: char, is_clr: &bool) -> char {

    if !is_clr {
      char('\r')
        .or(char('\n'))
        .parse_stream(input)
    } else {
      crlf()
        .or(char('\r'))
        .or(char('\n'))
        .parse_stream(input)
    }
  }
);

make_parser!(
  LineComment(input: char) -> () {
    value(())
      .skip(char('#'))
      .skip(many::<Vec<_>,_>(none_of("\r\n".chars())))
      .skip(LineTerminator::new(&true))
      .parse_stream(input)
  }
);

make_parser!(
  OperationDefinition(input: char) -> Operation {
    OperationTypeParser::new()
      .and(optional(NameParser::new()))
      .and(optional(VariableDefinitions::new()))
      .map(|((op_type,name),defns)| {
        let variable_definitions = match defns {
          Some(ds) => ds,
          None => Vec::new()
        };

        Operation::new(op_type, name, variable_definitions, Vec::new())
      })
      .skip(many::<Vec<_>,_>(or(WhiteSpace::new(), LineTerminator::new(&true))))
      .parse_stream(input)
  }
);

make_parser!(
  OperationTypeParser(input: char) -> OperationType {
    string("query").map(|_| OperationType::Query)
      .or(string("mutation").map(|_| OperationType::Mutation))
      .skip(many::<Vec<_>,_>(or(WhiteSpace::new(), LineTerminator::new(&true))))
      .parse_stream(input)
  }
);

make_parser!(
  NameParser(input: char) -> Name {
    or(letter(),char('_'))
      .map(|c| {
        let mut result = String::new();
        result.push(c);
        result
      })
      .and(many::<String,_>(alpha_num().or(char('_'))))
      .map(|(mut f,r)| {
        f.push_str(&r);
        f
      })
      .skip(many::<Vec<_>,_>(or(WhiteSpace::new(), LineTerminator::new(&true))))
      .parse_stream(input)
  }
);

make_parser!(
  VariableDefinitions(input: char) -> Vec<VariableDefinition> {
    between(char('('), char(')'), many(VariableDefinitionParser::new()))
      .parse_stream(input)
  }
);

make_parser!(
  VariableDefinitionParser(input: char) -> VariableDefinition {
    VariableParser::new()
      .skip(char(':'))
      .skip(many::<Vec<_>,_>(or(WhiteSpace::new(), LineTerminator::new(&true))))
      .and(TypeParser::new())
      .map(|(variable,var_type)| {
        VariableDefinition::new(variable, var_type, None)
      })
      .skip(many::<Vec<_>,_>(or(WhiteSpace::new(), LineTerminator::new(&true))))
      .parse_stream(input)
  }
);

make_parser!(
  VariableParser(input: char) -> Variable {
    char('$')
      .with(NameParser::new()) // skip the $
      .parse_stream(input)
  }
);

make_parser!(
  TypeParser(input: char) -> Type {
    let named_type = NameParser::new().map(Type::Named);
    let list_type = between(char('['),char(']'), TypeParser::new()).map(|t| Type::List(Box::new(t)));

// i can't clone for some reason -.-
    let non_null_type = char('!')
      .with(or(
        NameParser::new().map(Type::Named),
        between(char('['),char(']'), TypeParser::new()).map(|t| Type::List(Box::new(t)))
      ))
      .map(|t| Type::NonNull(Box::new(t)));

    non_null_type.or(named_type).or(list_type)
      .skip(many::<Vec<_>,_>(or(WhiteSpace::new(), LineTerminator::new(&true))))
      .parse_stream(input)
  }
);

make_parser!(
  Alias(input: char) -> Name {
    NameParser::new()
      .skip(char(':'))
      .skip(many::<Vec<_>,_>(or(WhiteSpace::new(), LineTerminator::new(&true))))
      .parse_stream(input)
  }
);

#[cfg(test)]
mod tests {
  use super::*;
  use combine::{State, Parser};

  macro_rules! assert_successful_parse {
    ($parser:ident,$input:expr,$result:expr) => {
      assert_eq!($parser::new().parse(State::new($input)).map(|x| x.0), Ok($result));
    }
  }

  #[test]
  fn test_parse_comment() {
    assert_successful_parse!(LineComment, "#hello world\r\n", ());
  }

  #[test]
  fn test_parse_operationtype() {
    assert_successful_parse!(OperationTypeParser, "query", OperationType::Query);
    assert_successful_parse!(OperationTypeParser, "mutation", OperationType::Mutation);
  }

  #[test]
  fn test_parse_name() {
    assert_successful_parse!(NameParser, "_asd", String::from("_asd"));
    assert_successful_parse!(NameParser, "aasd", String::from("aasd"));
    assert_successful_parse!(NameParser, "zasd", String::from("zasd"));
    assert_successful_parse!(NameParser, "Aasd", String::from("Aasd"));
    assert_successful_parse!(NameParser, "Zasd", String::from("Zasd"));
  }

  #[test]
  fn test_parse_alias() {
    assert_successful_parse!(Alias, "asd:", String::from("asd"));
    assert_successful_parse!(Alias, "asd :", String::from("asd"));
    assert_successful_parse!(Alias, "asd \r\n:", String::from("asd"));
  }

  #[test]
  fn test_parse_operation() {
    // named operation
    {
      let result = Operation::new(OperationType::Mutation,
                                  Some(String::from("test")),
                                  Vec::new(),
                                  Vec::new());
      assert_successful_parse!(OperationDefinition, "mutation test", result);
    }

    // non named
    {
      let result = Operation::new(OperationType::Mutation, None, Vec::new(), Vec::new());
      assert_successful_parse!(OperationDefinition, "mutation", result);
    }
  }

  #[test]
  fn test_parse_type() {
    assert_successful_parse!(TypeParser, "User", Type::Named(String::from("User")));
    assert_successful_parse!(TypeParser,
                             "[User]",
                             Type::List(Box::new(Type::Named(String::from("User")))));
  }

  #[test]
  fn test_parse_nonnull_type() {
    assert_successful_parse!(TypeParser,
                             "!User",
                             Type::NonNull(Box::new(Type::Named(String::from("User")))));
    assert_successful_parse!(TypeParser,
                             "![User]",
                             Type::NonNull(Box::new(Type::List(Box::new(Type::Named(String::from("User")))))));
  }

  #[test]
  fn test_parse_variabledefinition_nodefaultvalue() {
    assert_successful_parse!(VariableDefinitionParser,
                             "$devicePicSize: Int",
                             VariableDefinition::new(String::from("devicePicSize"), Type::Named(String::from("Int")), None));
  }
}
