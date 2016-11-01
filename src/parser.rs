// #![deny(missing_docs)]

use std::collections::HashMap;

use std::marker::PhantomData;

use combine::{Parser, ConsumedResult, Stream};
use combine::char::{tab, char, crlf, string, letter, alpha_num, digit};
use combine::combinator::{between, many, many1, none_of, one_of, or, optional, value, try, parser};
use combine::primitives::{StreamOnce, ParseError, Error, Consumed, Info};

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

    ($name:ident ($input_var:ident : $input_item_type:ty) -> $output_type:ty { $($tmpl:tt)+ } $($rest:tt)*) => {

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

        fn parse_lazy(&mut self, $input_var: I) -> ConsumedResult<Self::Output, Self::Input> {
          $($tmpl)+
        }
      }

      make_parser!($($rest)*);
    };

    ($name:ident ($input_var:ident : $input_item_type:ty , $($field:ident : &$typ:ty),*)
      -> $output_type:ty { $($tmpl:tt)+ } $($rest:tt)*) => {

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

          fn parse_lazy(&mut self, $input_var: I) -> ConsumedResult<Self::Output, Self::Input> {
            let &mut $name { _phantom, $($field),* } = self;
            $($tmpl)+
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
      .parse_lazy(input)
  }
);

make_parser!(
  LineTerminator(input: char, is_clr: &bool) -> char {

    let cr = char('\r');
    let lf = char('\n');

    if !is_clr {
      cr.clone()
        .or(lf)
        .parse_lazy(input)
    } else {
      crlf()
        .or(cr)
        .or(lf)
        .parse_lazy(input)
    }
  }
);

make_parser!(
  LineComment(input: char) -> () {
    value(())
      .skip(char('#'))
      .skip(many::<Vec<_>,_>(none_of("\r\n".chars())))
      .skip(LineTerminator::new(&true))
      .parse_lazy(input)
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
      .parse_lazy(input)
  }
);

make_parser!(
  OperationTypeParser(input: char) -> OperationType {
    string("query").map(|_| OperationType::Query)
      .or(string("mutation").map(|_| OperationType::Mutation))
      .skip(many::<Vec<_>,_>(or(WhiteSpace::new(), LineTerminator::new(&true))))
      .parse_lazy(input)
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
      .parse_lazy(input)
  }
);

make_parser!(
  VariableDefinitions(input: char) -> Vec<VariableDefinition> {
    between(char('('), char(')'), many(VariableDefinitionParser::new()))
      .parse_lazy(input)
  }
);

make_parser!(
  VariableDefinitionParser(input: char) -> VariableDefinition {
    VariableParser::new()
      .skip(char(':'))
      .skip(many::<Vec<_>,_>(or(WhiteSpace::new(), LineTerminator::new(&true))))
      .and(TypeParser::new())
      .and(optional(DefaultValue::new()))
      .map(|((variable,var_type),opt_type)| {
        VariableDefinition::new(variable, var_type, opt_type)
      })
      .skip(many::<Vec<_>,_>(or(WhiteSpace::new(), LineTerminator::new(&true))))
      .parse_lazy(input)
  }
);

make_parser!(
  VariableParser(input: char) -> Variable {
    char('$')
      .with(NameParser::new()) // skip the $
      .parse_lazy(input)
  }
);

make_parser!(
  TypeParser(input: char) -> Type {
    NonNullType::new().or(NamedType::new()).or(ListType::new())
      .skip(many::<Vec<_>,_>(or(WhiteSpace::new(), LineTerminator::new(&true))))
      .parse_lazy(input)
  }
);

make_parser!(
  NamedType(input: char) -> Type {
    NameParser::new()
      .map(Type::Named)
      .parse_lazy(input)
  }
);

make_parser!(
  ListType(input: char) -> Type {
    between(char('['),char(']'), TypeParser::new())
      .map(|t| Type::List(Box::new(t)))
      .parse_lazy(input)
  }
);

make_parser!(
  NonNullType(input: char) -> Type {
    char('!')
      .with(NamedType::new().or(ListType::new()))
      .map(|t| Type::NonNull(Box::new(t)))
      .parse_lazy(input)
  }
);

make_parser!(
  DefaultValue(input: char) -> Value {
    char('=')
      .skip(many::<Vec<_>,_>(or(WhiteSpace::new(), LineTerminator::new(&true))))
      .with(ValueParser::new(&false))
      .parse_lazy(input)
  }
);

make_parser!(
  ValueParser(input: char, constant: &bool) -> Value {

    let mut constants = try(FloatValue::new())
      .or(IntValue::new())
      .or(BooleanValue::new())
      .or(NullValue::new())
      .or(EnumValue::new())
      .or(ListValue::new(constant))
      .or(ObjectValue::new(constant));

    if *constant {
      VariableParser::new()
        .map(Value::Variable)
        .or(constants)
        .parse_lazy(input)
    } else {
      constants.parse_lazy(input)
    }
  }
);

make_parser!(
  IntPart(input: char) -> (Option<char>,String) {
    optional(char('-'))
      .and(
        or(
          char('0').map(|c: char| c.to_string()),
          one_of("123456789".chars())
            .map(|c: char| c.to_string())
            .and(many::<String,_>(digit()))
            .map(|(mut begin,rest)| {
              begin.push_str(&rest);
              begin
            })
        )
      )
      .parse_lazy(input)
  }
);

make_parser!(
  IntValue(input: char) -> Value {
    IntPart::new()
      .map(|(neg,number)| {
        match neg {
          Some(_) => -number.parse::<i32>().unwrap(),
          None => number.parse::<i32>().unwrap()
        }
      })
      .map(Value::Int)
      .skip(many::<Vec<_>,_>(or(WhiteSpace::new(), LineTerminator::new(&true))))
      .parse_lazy(input)
  }
);

make_parser!(
  FloatValue(input: char) -> Value {
    IntPart::new()
      .map(|(opt_sign, int_part)| {
        let mut result = String::new();
        if let Some(sign) = opt_sign {
          result.push(sign);
        }
        result.push_str(&int_part);
        result
      })
      .and(
        // FractionalPart ExponentialPart
        try(FractionalPart::new().map(Some).and(ExponentialPart::new().map(Some)))
          // FractionalPart
          .or(FractionalPart::new().map(Some).and(value(None)))
          // ExponentialPart
          .or(value(None).and(ExponentialPart::new().map(Some)))
      )
      .map(|(int_part,(opt_fract_part, opt_exp_part)) : (String, (Option<String>,Option<String>))| {
        let mut result = String::new();
        result.push_str(&int_part);

        println!("{:?} {:?} {:?}", int_part, opt_fract_part, opt_exp_part);

        match opt_fract_part {
          Some(fract_part) => {
            // add the fractional part first
            result.push_str(&fract_part);

            // if exponential part is there, we can just add it
            if let Some(exp_part) = opt_exp_part {
              result.push_str(&exp_part);
            }
          },
          None => {
            // to make rust parse it correctly, it needs to have a fractional number before the exponent part
            if let Some(exp_part) = opt_exp_part {
              result.push_str(".0");
              result.push_str(&exp_part);
            }
          }
        }
        // finally let rust parse it
        result.parse::<f32>().unwrap()
      })
      .map(Value::Float)
      .skip(many::<Vec<_>,_>(or(WhiteSpace::new(), LineTerminator::new(&true))))
      .parse_lazy(input)
  }
);

make_parser!(
  FractionalPart(input: char) -> String {
    char('.').map(|c: char| c.to_string())
      .and(many1::<String,_>(digit()))
      .map(|(mut result,s)| {
        result.push_str(&s);
        result
      })
      .parse_lazy(input)
  }
);

make_parser!(
  ExponentialPart(input: char) -> String {
    char('e').or(char('E'))
      .and(optional(char('+').or(char('-'))))
      .and(many1::<String,_>(digit()))
      .map(|((indicator,opt_sign),digits)| {
        let mut result = String::new();
        result.push(indicator);
        if let Some(sign) = opt_sign {
          result.push(sign);
        }
        result.push_str(&digits);
        result
      })
      .parse_lazy(input)
  }
);

make_parser!(
  BooleanValue(input: char) -> Value {
    string("true").map(|_| Value::Boolean(true))
      .or(string("false").map(|_| Value::Boolean(false)))
      .skip(many::<Vec<_>,_>(or(WhiteSpace::new(), LineTerminator::new(&true))))
      .parse_lazy(input)
  }
);

make_parser!(
  NullValue(input: char) -> Value {
    string("null")
      .map(|_| Value::Null)
      .skip(many::<Vec<_>,_>(or(WhiteSpace::new(), LineTerminator::new(&true))))
      .parse_lazy(input)
  }
);

// TODO enum parser: Name but not true or false or null
make_parser!(
  EnumValue(input: char) -> Value {

    let mut enum_value_parser = parser(|input| {
      let _: I   = input;
      let position = input.position();
      let (name,input) = try!(NameParser::new().parse_stream(input));

      match name.as_ref() {
        s @ "true" |
        s @ "false" |
        s @ "null" => {
          let mut errors = ParseError::empty(position);
          errors.add_error(Error::Unexpected(From::from(s.clone())));
          errors.add_error(Error::Expected(From::from("name")));
          Err(Consumed::Empty(errors))
        },

        s => Ok((Value::Enum(name.clone()),input))
      }
    });

    enum_value_parser
      .parse_lazy(input)
  }
);

make_parser!(
  ListValue(input: char, constant: &bool) -> Value {
    between(char('['), char(']'), many(ValueParser::new(constant)))
      .map(Value::List)
      .skip(many::<Vec<_>,_>(or(WhiteSpace::new(), LineTerminator::new(&true))))
      .parse_lazy(input)
  }
);

make_parser!(
  ObjectField(input: char, constant: &bool) -> (String, Value) {
    NameParser::new()
      .skip(char(':'))
      .skip(many::<Vec<_>,_>(or(WhiteSpace::new(), LineTerminator::new(&true))))
      .and(ValueParser::new(constant))
      .parse_lazy(input)
  }
);

make_parser!(
  ObjectValue(input: char, constant: &bool) -> Value {
    between(char('{'), char('}'), many::<Vec<_>,_>(ObjectField::new(constant)))
      .skip(many::<Vec<_>,_>(or(WhiteSpace::new(), LineTerminator::new(&true))))
      .map(|fields| {
        let mut result = HashMap::new();

        for (name,value) in fields.into_iter() {
          // TODO complain about same name fields?
          result.insert(name, value);
        }

        Value::Object(result)
      })
      .parse_lazy(input)
  }
);

// TODO .skip(many::<Vec<_>,_>(or(WhiteSpace::new(), LineTerminator::new(&true))))
// TODO add commas to this thing

make_parser!(
  Alias(input: char) -> Name {
    NameParser::new()
      .skip(char(':'))
      .skip(many::<Vec<_>,_>(or(WhiteSpace::new(), LineTerminator::new(&true))))
      .parse_lazy(input)
  }
);

#[cfg(test)]
mod tests {
  use super::*;
  use combine::{State, Parser};

  macro_rules! assert_successful_parse {
    ($parser:ident,$input:expr,$expected:expr) => {
      let result = $parser::new().parse(State::new($input)).map(|x| x.0);
      println!("Input({:?}) Result({:?}) Expected(Ok({:?}))", $input, result, $expected);
      assert_eq!(result, Ok($expected))
    };

    ($parser:expr,$input:expr,$expected:expr) => {
      let result = $parser.parse(State::new($input)).map(|x| x.0);
      println!("Input({:?}) Result({:?}) Expected(Ok({:?}))", $input, result, $expected);
      assert_eq!(result, Ok($expected))
    };
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
                             VariableDefinition::new(String::from("devicePicSize"),
                                                     Type::Named(String::from("Int")),
                                                     None));
  }

  #[test]
  fn test_parse_variabledefinition_defaultvalue() {
    assert_successful_parse!(VariableDefinitionParser,
                             "$devicePicSize: Int = 10",
                             VariableDefinition::new(String::from("devicePicSize"),
                                                     Type::Named(String::from("Int")),
                                                     Some(Value::Int(10))));

    assert_successful_parse!(VariableDefinitionParser,
                             "$devicePicSize: Float = 1.0",
                             VariableDefinition::new(String::from("devicePicSize"),
                                                     Type::Named(String::from("Float")),
                                                     Some(Value::Float(1.0))));
  }

  #[test]
  fn test_parse_intvalue() {
    assert_successful_parse!(IntValue, "0", Value::Int(0));
    assert_successful_parse!(IntValue, "-0", Value::Int(0));
    assert_successful_parse!(IntValue, "1", Value::Int(1));
    assert_successful_parse!(IntValue, "-1", Value::Int(-1));
    assert_successful_parse!(IntValue, "10", Value::Int(10));
    assert_successful_parse!(IntValue, "-10", Value::Int(-10));
  }

  #[test]
  fn test_parse_floatvalue() {
    // test .
    assert_successful_parse!(FloatValue, "0.1", Value::Float(0.1));

    // test optional fract_part
    assert_successful_parse!(FloatValue, "10e1", Value::Float(10.0e1));
    assert_successful_parse!(FloatValue, "10.0e1", Value::Float(10.0e1));

    // test case e
    assert_successful_parse!(FloatValue, "10E1", Value::Float(10.0e1));
    assert_successful_parse!(FloatValue, "10.0E1", Value::Float(10.0e1));

    // test signs
    assert_successful_parse!(FloatValue, "10e+1", Value::Float(10.0e1));
    assert_successful_parse!(FloatValue, "10.0e-1", Value::Float(10.0e-1));
  }

  #[test]
  fn test_parse_const_listvalue() {
    assert_successful_parse!(ListValue::new(&true), "[null]", Value::List(vec![Value::Null]));
    assert_successful_parse!(ListValue::new(&true), "[null true false]", Value::List(vec![Value::Null, Value::Boolean(true), Value::Boolean(false)]));
  }

  // TODO add tests for object value
}
