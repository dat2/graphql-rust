// #![deny(missing_docs)]

use std::collections::HashMap;
use std::marker::PhantomData;

use combine::{Parser, ConsumedResult, Stream};
use combine::char::{tab, char, crlf, string, letter, alpha_num, digit, hex_digit};
use combine::combinator::{between, many, many1, none_of, one_of, or, optional, value, try, parser};
use combine::primitives::{ParseError, Error, Consumed};

use ast::*;

// ===========================================================================
// The main make_parser macro
// ===========================================================================
macro_rules! make_parser {

// base case
    () => {};

    ($name:ident ($input_var:ident : $input_item_type:ty) -> $output_type:ty {
        $($tmpl:tt)+
      }
      $($rest:tt)*) => {

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
// https://github.com/facebook/graphql/issues/214
// https://github.com/facebook/graphql/pull/231

// http://facebook.github.io/graphql/#SourceCharacter
// graphql :: [0009,000A,000D, [0020,FFFF]]

// https://doc.rust-lang.org/std/char/
// http://www.unicode.org/glossary/#unicode_scalar_value
// char :: [0,D7FF] u [E000,10FFFF]

// ===========================================================================
// Utility Functions
// ===========================================================================
fn or_empty<T>(opt_vec: Option<Vec<T>>) -> Vec<T> {
  match opt_vec {
    Some(vec) => vec,
    None => Vec::new(),
  }
}

// ===========================================================================
// 2.1 Lexers
// ===========================================================================
macro_rules! ignore_parser {
  () => (
    many::<Vec<_>,_>(
      WhiteSpace::new()
        .or(LineTerminator::new(&true))
        .or(LineComment::new())
      )
  )
}

make_parser!(
  WhiteSpace(input: char) -> () {
    value(())
      .skip(char(' ').or(tab()))
      .parse_lazy(input)
  }
);

make_parser!(
  LineTerminator(input: char, is_clr: &bool) -> () {

    let cr = char('\r');
    let lf = char('\n');

    if !is_clr {
      value(())
        .skip(cr.or(lf))
        .parse_lazy(input)
    } else {
      value(())
        .skip(crlf().or(cr).or(lf))
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
  NameParser(input: char) -> Name {
    or(letter(),char('_'))
      .map(|c: char| c.to_string())
      .and(many::<String,_>(alpha_num().or(char('_'))))
      .map(|(mut f,r)| {
        f.push_str(&r);
        f
      })
      .skip(ignore_parser!())
      .parse_lazy(input)
  }
);

// ===========================================================================
// 2.2 Document Parsers
// ===========================================================================

// http://facebook.github.io/graphql/#sec-Language.Query-Document
// Query shorthand
//
// If a document contains only one query operation, and that query defines no
// variables and contains no directives, that operation may be represented in a
// short‐hand form which omits the query keyword and query name.
make_parser!(
  DocumentParser(input: char) -> Document {
    try(SelectionSet::new().map(|selection_set| {
      vec![
        Definition::Operation(
          Operation::new(OperationType::Query,None,Vec::new(),Vec::new(),selection_set)
        )
      ]
    }))
      .or(many1::<Vec<_>,_>(
        OperationDefinition::new().map(Definition::Operation)
          .or(FragmentDefinition::new().map(Definition::Fragment))))
      .map(|defns| {
        Document::new(defns)
      })
      .parse_lazy(input)
  }
);

// ===========================================================================
// 2.3 Operation Parsers
// ===========================================================================

make_parser!(
  OperationDefinition(input: char) -> Operation {
    OperationTypeParser::new()
      .and(optional(NameParser::new()))
      .and(optional(VariableDefinitions::new()))
      .and(optional(Directives::new()))
      .and(SelectionSet::new())
      .map(|((((op_type,name),opt_variable_definitions),opt_directives),selection_set)| {
        let variable_definitions = or_empty(opt_variable_definitions);
        let directives = or_empty(opt_directives);
        Operation::new(op_type, name, variable_definitions, directives, selection_set)
      })
      .skip(ignore_parser!())
      .parse_lazy(input)
  }
);

make_parser!(
  OperationTypeParser(input: char) -> OperationType {
    string("query").map(|_| OperationType::Query)
      .or(string("mutation").map(|_| OperationType::Mutation))
      .skip(ignore_parser!())
      .parse_lazy(input)
  }
);

// ===========================================================================
// 2.4 Selection Set Parsers
// ===========================================================================
make_parser!(
  SelectionSet(input: char) -> Vec<Selection> {
    between(
      char('{').skip(ignore_parser!()),
      char('}').skip(ignore_parser!()),
      many::<Vec<_>,_>(SelectionParser::new())
    )
      .parse_lazy(input)
  }
);

make_parser!(
  SelectionParser(input: char) -> Selection {
      FieldParser::new().map(Selection::Field)
        .or(try(FragmentSpreadParser::new()).map(Selection::FragmentSpread))
        .or(InlineFragmentParser::new().map(Selection::InlineFragment))
        .parse_lazy(input)
  }
);

// ===========================================================================
// 2.5 Field Parser
// ===========================================================================
make_parser!(
  FieldParser(input: char) -> Field {
    optional(try(Alias::new()))
      .and(NameParser::new())
      .and(optional(Arguments::new()))
      .and(optional(Directives::new()))
      .and(optional(SelectionSet::new()))
      .map(|((((opt_alias,name),opt_arguments),opt_directives),opt_selection_set)| {
        let arguments = or_empty(opt_arguments);
        let directives = or_empty(opt_directives);
        let selection_set = or_empty(opt_selection_set);
        Field::new(opt_alias,name,arguments,directives,selection_set)
      })
      .skip(ignore_parser!())
      .parse_lazy(input)
  }
);

// ===========================================================================
// 2.6 Arguments Parsers
// ===========================================================================
make_parser!(
  Arguments(input: char) -> Vec<Argument> {
    between(char('('), char(')'), many::<Vec<_>,_>(ArgumentParser::new()))
      .parse_lazy(input)
  }
);

make_parser!(
  ArgumentParser(input: char) -> Argument {
    NameParser::new()
      .skip(char(':'))
      .skip(ignore_parser!())
      .and(ValueParser::new(&false))
      .map(|(name,value)| Argument::new(name,value))
      .parse_lazy(input)
  }
);
// ===========================================================================
// 2.7 Alias Parser
// ===========================================================================

make_parser!(
  Alias(input: char) -> Name {
    NameParser::new()
      .skip(char(':'))
      .skip(ignore_parser!())
      .parse_lazy(input)
  }
);

// ===========================================================================
// 2.8 Fragment Parsers
// ===========================================================================

make_parser!(
  FragmentSpreadParser(input: char) -> FragmentSpread {
    string("...")
      .with(FragmentNameParser::new())
      .and(optional(Directives::new()))
      .map(|(fragment_name, opt_directives)| {
        FragmentSpread::new(fragment_name, or_empty(opt_directives))
      })
      .skip(ignore_parser!())
      .parse_lazy(input)
  }
);

make_parser!(
  FragmentNameParser(input: char) -> Name {

    let mut fragment_name_parser = parser(|input| {
      let _: I  = input;
      let position = input.position();
      let (name,input) = try!(NameParser::new().parse_stream(input));

      if name == String::from("on")
      {
        let mut errors = ParseError::empty(position);
        errors.add_error(Error::Unexpected(From::from(name.clone())));
        errors.add_error(Error::Expected(From::from("name")));
        Err(Consumed::Empty(errors))
      } else {
        Ok((name.clone(),input))
      }
    });

    fragment_name_parser
      .parse_lazy(input)
  }
);

make_parser!(
  InlineFragmentParser(input: char) -> InlineFragment {
    string("...")
      .skip(ignore_parser!())
      .with(optional(TypeCondition::new()))
      .and(optional(Directives::new()))
      .and(SelectionSet::new())
      .map(|((opt_type_condition,opt_directives),selection_set)| {
        InlineFragment::new(opt_type_condition,or_empty(opt_directives),selection_set)
      })
      .parse_lazy(input)
  }
);

make_parser!(
  TypeCondition(input: char) -> Type {
    string("on")
      .skip(ignore_parser!())
      .with(NameParser::new())
      .map(Type::Named)
      .parse_lazy(input)
  }
);

make_parser!(
  FragmentDefinition(input: char) -> Fragment {
    string("fragment")
      .skip(ignore_parser!())
      .with(FragmentNameParser::new())
      .and(TypeCondition::new())
      .and(optional(Directives::new()))
      .and(SelectionSet::new())
      .map(|(((fragment_name,type_condition),opt_directives),selection_set)| {
        Fragment::new(fragment_name,type_condition,or_empty(opt_directives),selection_set)
      })
      .parse_lazy(input)
  }
);


// ===========================================================================
// 2.9 Value Parsers
// ===========================================================================

make_parser!(
  ValueParser(input: char, constant: &bool) -> Value {

    let mut constants = try(FloatValue::new())
      .or(IntValue::new())
      .or(BooleanValue::new())
      .or(NullValue::new())
      .or(EnumValue::new())
      .or(ListValue::new(constant))
      .or(ObjectValue::new(constant));

    if !*constant {
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
      .skip(ignore_parser!())
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
      .skip(ignore_parser!())
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
      .skip(ignore_parser!())
      .parse_lazy(input)
  }
);

make_parser!(
  StringValue(input: char) -> Value {
    between(
      char('"'),
      char('"'),
      many::<Vec<String>,_>(
        try(EscapedUnicode::new())
          .or(try(EscapedCharacter::new()))
          .or(none_of("\"\\\r\n".chars()).map(|c: char| c.to_string()))
      )
    )
      .map(|vec| {
        vec.iter()
          .cloned()
          .fold(String::from(""), |mut acc,s| {
            acc.push_str(&s);
            acc
          })
      })
      .map(Value::String)
      .skip(ignore_parser!())
      .parse_lazy(input)
  }
);

fn hex_digit_to_u8(input: char) -> u8 {
  match input {
    '0'...'9' => (input as u8) - ('0' as u8),
    'a'...'f' => (input as u8) - ('a' as u8) + 10,
    'A'...'F' => (input as u8) - ('A' as u8) + 10,
    _ => 0,
  }
}

make_parser!(
  EscapedUnicode(input: char) -> String {
    string("\\u")
      .with(
        hex_digit().and(hex_digit()).and(hex_digit()).and(hex_digit())
          .map(|(((b1,b2),b3),b4)| {
            let left = hex_digit_to_u8(b1) << 4 ^ hex_digit_to_u8(b2);
            let right = hex_digit_to_u8(b3) << 4 ^ hex_digit_to_u8(b4);

            let mut bytes = vec![];
            if left != 0 {
              bytes.push(left);
            }
            if right != 0 {
              bytes.push(right);
            }

            String::from_utf8(bytes).unwrap()
          })
      )
      .parse_lazy(input)
  }
);

make_parser!(
  EscapedCharacter(input: char) -> String {
    char('\\')
      .and(one_of("\"/\\bfnrt".chars()))
      .map(|(_,b)| {
        match b {
          'b' => String::from("\x08"),
          'f' => String::from("\x0C"),
          'n' => String::from("\n"),
          'r' => String::from("\r"),
          't' => String::from("\t"),
          c => {
            let mut result = String::new();
            result.push(c);
            result
          }
        }
      })
      .parse_lazy(input)
  }
);

make_parser!(
  NullValue(input: char) -> Value {
    string("null")
      .map(|_| Value::Null)
      .skip(ignore_parser!())
      .parse_lazy(input)
  }
);

make_parser!(
  EnumValue(input: char) -> Value {

    let mut enum_value_parser = parser(|input| {
      let _: I  = input;
      let position = input.position();
      let (name,input) = try!(NameParser::new().parse_stream(input));

      if    name == String::from("true")
         || name == String::from("false")
         || name == String::from("null")
      {
        let mut errors = ParseError::empty(position);
        errors.add_error(Error::Unexpected(From::from(name.clone())));
        errors.add_error(Error::Expected(From::from("name")));
        Err(Consumed::Empty(errors))
      } else {
        Ok((Value::Enum(name.clone()),input))
      }
    });

    enum_value_parser
      .parse_lazy(input)
  }
);

make_parser!(
  ListValue(input: char, constant: &bool) -> Value {
    between(
      char('[').skip(ignore_parser!()),
      char(']').skip(ignore_parser!()),
      many(ValueParser::new(constant))
    )
      .map(Value::List)
      .parse_lazy(input)
  }
);

make_parser!(
  ObjectField(input: char, constant: &bool) -> (String, Value) {
    NameParser::new()
      .skip(char(':'))
      .skip(ignore_parser!())
      .and(ValueParser::new(constant))
      .parse_lazy(input)
  }
);

make_parser!(
  ObjectValue(input: char, constant: &bool) -> Value {
    between(
      char('{').skip(ignore_parser!()),
      char('}').skip(ignore_parser!()),
      many::<Vec<_>,_>(ObjectField::new(constant))
    )
      .map(|fields| {
        let mut result = HashMap::new();

// TODO complain about same name fields?
        for (name,value) in fields.into_iter() {
          result.insert(name, value);
        }

        Value::Object(result)
      })
      .parse_lazy(input)
  }
);


// ===========================================================================
// 2.10 Variable Parsers
// ===========================================================================

make_parser!(
  VariableParser(input: char) -> Variable {
    char('$')
      .with(NameParser::new()) // skip the $
      .parse_lazy(input)
  }
);

make_parser!(
  VariableDefinitions(input: char) -> Vec<VariableDefinition> {
    between(char('('), char(')').skip(ignore_parser!()), many(VariableDefinitionParser::new()))
      .parse_lazy(input)
  }
);

make_parser!(
  VariableDefinitionParser(input: char) -> VariableDefinition {
    VariableParser::new()
      .skip(char(':'))
      .skip(ignore_parser!())
      .and(TypeParser::new())
      .and(optional(DefaultValue::new()))
      .map(|((variable,var_type),opt_type)| {
        VariableDefinition::new(variable, var_type, opt_type)
      })
      .skip(ignore_parser!())
      .parse_lazy(input)
  }
);

make_parser!(
  DefaultValue(input: char) -> Value {
    char('=')
      .skip(ignore_parser!())
      .with(ValueParser::new(&true))
      .parse_lazy(input)
  }
);

// ===========================================================================
// 2.11 Type Parsers
// ===========================================================================

make_parser!(
  TypeParser(input: char) -> Type {
    NonNullType::new().or(NamedType::new()).or(ListType::new())
      .skip(ignore_parser!())
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
    between(char('[').skip(ignore_parser!()),char(']').skip(ignore_parser!()), TypeParser::new())
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

// ===========================================================================
// 2.12 Directive Parsers
// ===========================================================================
make_parser!(
  Directives(input: char) -> Vec<Directive> {
    many::<Vec<_>,_>(DirectiveParser::new())
      .parse_lazy(input)
  }
);

make_parser!(
  DirectiveParser(input: char) -> Directive {
    char('@')
      .with(NameParser::new())
      .and(optional(Arguments::new()))
      .map(|(name,opt_args)| Directive::new(name, or_empty(opt_args)))
      .skip(ignore_parser!())
      .parse_lazy(input)
  }
);

#[cfg(test)]
mod tests {
  use super::*;
  use ast::*;
  use combine::{State, Parser};

  use std::collections::HashMap;

  macro_rules! assert_successful_parse {
    // base case
    () => {};

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

  // ===========================================================================
  // 2.1 Lexer Tests
  // ===========================================================================
  #[test]
  fn test_parse_comment() {
    assert_successful_parse!(LineComment, "#hello world\r\n", ());
  }

  #[test]
  fn test_parse_name() {
    assert_successful_parse!(NameParser, "_asd", String::from("_asd"));
    assert_successful_parse!(NameParser, "aasd", String::from("aasd"));
    assert_successful_parse!(NameParser, "zasd", String::from("zasd"));
    assert_successful_parse!(NameParser, "Aasd", String::from("Aasd"));
    assert_successful_parse!(NameParser, "Zasd", String::from("Zasd"));
  }

  // ===========================================================================
  // 2.2 Document Tests
  // ===========================================================================

  // ===========================================================================
  // 2.3 Operation Tests
  // ===========================================================================

  #[test]
  fn test_parse_operationtype() {
    assert_successful_parse!(OperationTypeParser, "query", OperationType::Query);
    assert_successful_parse!(OperationTypeParser, "mutation", OperationType::Mutation);
  }

  #[test]
  fn test_parse_operation_name() {
    // named operation
    {
      let result = Operation::new(OperationType::Mutation,
                                  Some(String::from("test")),
                                  Vec::new(),
                                  Vec::new(),
                                  Vec::new());
      assert_successful_parse!(OperationDefinition, "mutation test { }", result);
    }

    // non named
    {
      let result = Operation::new(OperationType::Mutation,
                                  None,
                                  Vec::new(),
                                  Vec::new(),
                                  Vec::new());
      assert_successful_parse!(OperationDefinition, "mutation { }", result);
    }
  }

  #[test]
  fn test_parse_operation_variables() {
    // operation with variable definitions
    let result = Operation::new(OperationType::Query,
                                Some(String::from("likeStory")),
                                vec![VariableDefinition::new(String::from("storyID"),
                                                             Type::Named(String::from("Int")),
                                                             None)],
                                Vec::new(),
                                Vec::new());
    assert_successful_parse!(OperationDefinition,
                             "query likeStory($storyID: Int) { }",
                             result);
  }

  #[test]
  fn test_parse_operation_directives() {
    // operation with directives
    let result = Operation::new(OperationType::Query,
                                Some(String::from("likeStory")),
                                Vec::new(),
                                vec![Directive::new(String::from("dir"), Vec::new())],
                                Vec::new());
    assert_successful_parse!(OperationDefinition, "query likeStory @dir { }", result);
  }

  #[test]
  fn test_parse_operation_selectionset() {
    // operation with selection set
    let result =
      Operation::new(OperationType::Query,
                     Some(String::from("likeStory")),
                     Vec::new(),
                     Vec::new(),
                     vec![Selection::Field(Field::new(None, String::from("id"), Vec::new(), Vec::new(), Vec::new()))]);
    assert_successful_parse!(OperationDefinition, "query likeStory { id }", result);
  }

  // ===========================================================================
  // 2.4 Selection Set Tests
  // ===========================================================================

  #[test]
  fn test_parse_selectionset_fields() {
    // selection set with fields only
    let result = vec![Selection::Field(Field::new(None, String::from("id"), Vec::new(), Vec::new(), Vec::new()))];

    assert_successful_parse!(SelectionSet, "{ id }", result);
  }

  #[test]
  fn test_parse_selectionset_fragmentspread() {
    // selection set with fragment only
    let result = vec![Selection::FragmentSpread(FragmentSpread::new(String::from("friendsFragment"), Vec::new()))];

    assert_successful_parse!(SelectionSet, "{ ...friendsFragment }", result);
  }

  #[test]
  fn test_parse_selectionset_inlinefragment() {
    // selection set with inline fragment only
    let result = vec![Selection::InlineFragment(InlineFragment::new(Some(Type::Named(String::from("User"))),
                                                                    Vec::new(),
                                                                    vec![Selection::Field(Field::new(None,
                                                                                          String::from("id"),
                                                                                          Vec::new(),
                                                                                          Vec::new(),
                                                                                          Vec::new()))]))];

    assert_successful_parse!(SelectionSet, "{ ... on User { id } }", result);
  }

  // ===========================================================================
  // 2.5 Fields Tests
  // ===========================================================================

  #[test]
  fn test_parse_field_simple() {
    // field with only name
    let result = Field::new(None, String::from("id"), Vec::new(), Vec::new(), Vec::new());

    assert_successful_parse!(FieldParser, "id", result);
  }

  #[test]
  fn test_parse_field_alias() {
    // field with an alias
    let result = Field::new(Some(String::from("alias")),
                            String::from("id"),
                            Vec::new(),
                            Vec::new(),
                            Vec::new());

    assert_successful_parse!(FieldParser, "alias: id", result);
  }

  #[test]
  fn test_parse_field_arguments() {
    // field iwht arguments
    let result = Field::new(None,
                            String::from("profilePic"),
                            vec![Argument::new(String::from("size"), Value::Int(100))],
                            Vec::new(),
                            Vec::new());

    assert_successful_parse!(FieldParser, "profilePic(size: 100)", result);
  }

  #[test]
  fn test_parse_field_directives() {
    // field with directives
    let result = Field::new(None,
                            String::from("id"),
                            Vec::new(),
                            vec![Directive::new(String::from("test"), Vec::new())],
                            Vec::new());

    assert_successful_parse!(FieldParser, "id @test", result);
  }

  #[test]
  fn test_parse_field_selectionset() {
    // field with a sub selection set
    let result =
      Field::new(None,
                 String::from("me"),
                 Vec::new(),
                 Vec::new(),
                 vec![Selection::Field(Field::new(None, String::from("id"), Vec::new(), Vec::new(), Vec::new()))]);

    assert_successful_parse!(FieldParser, "me { id }", result);
  }

  // ===========================================================================
  // 2.6 Arguments Tests
  // ===========================================================================

  #[test]
  fn test_parse_arguments() {
    assert_successful_parse!(Arguments,
                             "(x:1 y:2)",
                             vec![Argument::new(String::from("x"), Value::Int(1)),
                                  Argument::new(String::from("y"), Value::Int(2))]);
  }

  #[test]
  fn test_parse_argument() {
    assert_successful_parse!(ArgumentParser,
                             "x:1",
                             Argument::new(String::from("x"), Value::Int(1)));
  }

  // ===========================================================================
  // 2.7 Alias Tests
  // ===========================================================================

  #[test]
  fn test_parse_alias() {
    assert_successful_parse!(Alias, "asd:", String::from("asd"));
    assert_successful_parse!(Alias, "asd :", String::from("asd"));
    assert_successful_parse!(Alias, "asd \r\n:", String::from("asd"));
  }

  // ===========================================================================
  // 2.8 Fragments Tests
  // ===========================================================================

  #[test]
  fn test_parse_fragmentspread() {
    assert_successful_parse!(FragmentSpreadParser,
                             "...friendsFragment",
                             FragmentSpread::new(String::from("friendsFragment"), Vec::new()));
  }

  #[test]
  fn test_parse_fragmentspread_directives() {
    assert_successful_parse!(FragmentSpreadParser,
                             "...friendsFragment @test",
                             FragmentSpread::new(String::from("friendsFragment"),
                                                 vec![Directive::new(String::from("test"), Vec::new())]));
  }

  #[test]
  fn test_parse_fragmentdefinition() {
    assert_successful_parse!(FragmentDefinition,
                             "fragment friendsFragment on User { }",
                             Fragment::new(String::from("friendsFragment"),
                                           Type::Named(String::from("User")),
                                           Vec::new(),
                                           Vec::new()));
  }

  #[test]
  fn test_parse_fragmentdefinition_directives() {
    assert_successful_parse!(FragmentDefinition,
                             "fragment friendsFragment on User @test { }",
                             Fragment::new(String::from("friendsFragment"),
                                           Type::Named(String::from("User")),
                                           vec![Directive::new(String::from("test"), Vec::new())],
                                           Vec::new()));
  }

  #[test]
  fn test_parse_fragmentdefinition_selectionset() {
    assert_successful_parse!(FragmentDefinition,
                             "fragment friendsFragment on User { friends }",
                             Fragment::new(String::from("friendsFragment"),
                                           Type::Named(String::from("User")),
                                           Vec::new(),
                                           vec![Selection::Field(Field::new(None,
                                                                            String::from("friends"),
                                                                            Vec::new(),
                                                                            Vec::new(),
                                                                            Vec::new()))]));
  }

  #[test]
  fn test_parse_typecondition() {
    assert_successful_parse!(TypeCondition, "on User", Type::Named(String::from("User")));
  }

  #[test]
  fn test_parse_inlinefragment() {
    assert_successful_parse!(InlineFragmentParser,
                             "... {}",
                             InlineFragment::new(None, Vec::new(), Vec::new()));
  }

  #[test]
  fn test_parse_inlinefragment_typecondition() {
    assert_successful_parse!(InlineFragmentParser,
                             "... on User {}",
                             InlineFragment::new(Some(Type::Named(String::from("User"))),
                                                 Vec::new(),
                                                 Vec::new()));
  }

  #[test]
  fn test_parse_inlinefragment_directives() {
    assert_successful_parse!(InlineFragmentParser,
                             "... @test {}",
                             InlineFragment::new(None,
                                                 vec![Directive::new(String::from("test"), Vec::new())],
                                                 Vec::new()));
  }

  // ===========================================================================
  // 2.9 Value Tests
  // ===========================================================================

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
    assert_successful_parse!(ListValue::new(&true),
                             "[null]",
                             Value::List(vec![Value::Null]));
    assert_successful_parse!(ListValue::new(&true),
                             "[null true false]",
                             Value::List(vec![Value::Null, Value::Boolean(true), Value::Boolean(false)]));
  }

  #[test]
  fn test_parse_enumvalue_failure() {
    // it should fail to parse true
    {
      let result = EnumValue::new().parse(State::new("true")).map(|x| x.0);
      match result {
        Err(err) => {
          assert!(format!("{}", err).contains("Unexpected `true`"));
        }
        // it should be an error
        _ => assert!(false),
      }
    }

    // it should fail to parse false
    {
      let result = EnumValue::new().parse(State::new("false")).map(|x| x.0);
      match result {
        Err(err) => {
          assert!(format!("{}", err).contains("Unexpected `false`"));
        }
        // it should be an error
        _ => assert!(false),
      }
    }

    // it should fail to parse null
    {
      let result = EnumValue::new().parse(State::new("null")).map(|x| x.0);
      match result {
        Err(err) => {
          assert!(format!("{}", err).contains("Unexpected `null`"));
        }
        // it should be an error
        _ => assert!(false),
      }
    }
  }

  #[test]
  fn test_parse_enumvalue_successful() {
    assert_successful_parse!(EnumValue, "test", Value::Enum(String::from("test")));
  }

  #[test]
  fn test_parse_objectvalue_empty() {
    assert_successful_parse!(ObjectValue::new(&true), "{}", Value::Object(HashMap::new()));
  }

  #[test]
  fn test_parse_objectvalue_onefield() {
    let mut map = HashMap::new();
    map.insert(String::from("x"), Value::Int(1));
    let value = Value::Object(map);

    assert_successful_parse!(ObjectValue::new(&true), "{ x : 1 }", value);
  }

  #[test]
  fn test_parse_string_unicodeescape() {
    // unicode string
    assert_successful_parse!(StringValue, r#""\u0025""#, Value::String(String::from("%")));
    assert_successful_parse!(StringValue, r#""\u0040""#, Value::String(String::from("@")));
  }

  #[test]
  fn test_parse_string_escaped() {
    assert_successful_parse!(StringValue, r#""\"""#, Value::String(String::from("\"")));
    assert_successful_parse!(StringValue, r#""\\""#, Value::String(String::from("\\")));
    assert_successful_parse!(StringValue, r#""\/""#, Value::String(String::from("/")));
    assert_successful_parse!(StringValue, r#""\b""#, Value::String(String::from("\x08")));
    assert_successful_parse!(StringValue, r#""\f""#, Value::String(String::from("\x0C")));
    assert_successful_parse!(StringValue, r#""\n""#, Value::String(String::from("\n")));
    assert_successful_parse!(StringValue, r#""\r""#, Value::String(String::from("\r")));
    assert_successful_parse!(StringValue, r#""\t""#, Value::String(String::from("\t")));
  }

  #[test]
  fn test_parse_stringvalue() {
    // empty string
    assert_successful_parse!(StringValue, r#""""#, Value::String(String::from("")));

    // strings with random stuff in it
    assert_successful_parse!(StringValue,
                             r#""hello world""#,
                             Value::String(String::from("hello world")));
    assert_successful_parse!(StringValue,
                             r#""hello \u0025""#,
                             Value::String(String::from("hello %")));
    assert_successful_parse!(StringValue,
                             r#""hello\n\u0025""#,
                             Value::String(String::from("hello\n%")));
  }

  // ===========================================================================
  // 2.10 Variables Tests
  // ===========================================================================
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

  // ===========================================================================
  // 2.11 Type Tests
  // ===========================================================================

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

  // ===========================================================================
  // 2.12 Directive Tests
  // ===========================================================================

  #[test]
  fn test_parse_directive() {
    // directive no arguments
    assert_successful_parse!(DirectiveParser,
                             "@dir",
                             Directive::new(String::from("dir"), Vec::new()));

    // directive with arguments
    assert_successful_parse!(DirectiveParser,
                             "@dir(x:1)",
                             Directive::new(String::from("dir"),
                                            vec![Argument::new(String::from("x"), Value::Int(1))]));
  }

  #[test]
  fn test_parse_directives() {
    // multiple directives
    assert_successful_parse!(Directives,
                             "@dir\n@dir2(x:1)",
                             vec![Directive::new(String::from("dir"), Vec::new()),
                                  Directive::new(String::from("dir2"),
                                                 vec![Argument::new(String::from("x"), Value::Int(1))])]);
  }
}
