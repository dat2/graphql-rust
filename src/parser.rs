use std::collections::HashMap;

use std::str::Chars;
use std::iter::FromIterator;
use std::marker::PhantomData;


use combine::{Parser, ParseResult, Stream, parser};
use combine::char::{CrLf, Tab, tab, char, crlf};
use combine::combinator::{Many, FnParser, NoneOf, Skip, Or, Token, or, many, none_of};

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

#[derive(Debug,PartialEq)]
pub struct Fragment {
    name: Name,
    type_condition: Type,
    directives: Vec<Directive>,
    selection_set: SelectionSet,
}

// ok wtf is going on here
type WhiteSpace<I> = Or<Token<I>, Tab<I>>;
pub fn white_space<I: Stream<Item = char>>() -> WhiteSpace<I> {
    char(' ').or(tab())
}

// https://doc.rust-lang.org/error-index.html#E0207
// struct LineTerminator<T> {
//   phantom: PhantomData<T>,
// }

// impl<T> LineTerminator<T> {
//   fn new() -> Self {
//     LineTerminator {
//       phantom: PhantomData
//     }
//   }
// }


macro_rules! make_parser_struct {
  ($name: ident) => {
    pub struct $name<T> {
      phantom: PhantomData<T>,
    }

    impl<T> $name<T> {
      fn new() -> Self {
        $name {
          phantom: PhantomData
        }
      }
    }
  }
}

macro_rules! make_parser {

    // base case
    () => {};

    ($name:ident ($input_var:ident : $input_item_type:ty) -> $output_type:ty { $($tmpl:tt)* } $($rest:tt)*) => {

      pub struct $name<T> {
        phantom: PhantomData<T>,
      }

      impl<T> $name<T> {
          pub fn new() -> Self {
              $name {
                phantom: PhantomData
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

        pub struct $name<'a, T> {
          phantom: PhantomData<T>,
          $( $field: &'a $typ),*
        }

        impl<'a, T> $name<'a, T> {
          pub fn new($($field: &'a $typ),*) -> Self {
            $name {
              phantom: PhantomData,
              $( $field: $field),*
            }
          }
        }

        impl<'a, I> Parser for $name<'a, I> where I: Stream<Item=$input_item_type> {
          type Input = I;
          type Output = $output_type;

          fn parse_stream(&mut self, $input_var: I) -> ParseResult<Self::Output, Self::Input> {
            let &mut $name { phantom, $($field),* } = self;

            $($tmpl)*
          }
        }

        make_parser!($($rest)*);
    };

}

// make_parser_struct!(LineTerminator);

// impl<I> Parser for LineTerminator<I> where I: Stream<Item=char> {
//   type Input = I;
//   type Output = char;

//   fn parse_stream(&mut self, input: I) -> ParseResult<Self::Output, Self::Input> {
//     crlf()
//       .or(char('\r'))
//       .or(char('\n'))
//       .parse_stream(input)
//   }
// }

// make_parser_struct!(Comment);

// impl<I> Parser for Comment<I> where I: Stream<Item=char> {
//   type Input = I;
//   type Output = char;

//   fn parse_stream(&mut self, input: I) -> ParseResult<Self::Output, Self::Input> {
//     char('#')
//       .skip(LineTerminator::new())
//       .parse_stream(input)
//   }
// }

make_parser!(
  LineTerminator(input: char, is_clr: &bool) -> char {

    if !is_clr {
      return crlf()
        .or(char('\r'))
        .parse_stream(input);
    }

    crlf()
      .or(char('\r'))
      .or(char('\n'))
      .parse_stream(input)
  }
);

make_parser!(
  Comment(input: char) -> char {
    char('#')
      .skip(LineTerminator::new(&true))
      .parse_stream(input)
  }
);

// TODO: combine definitions
// make_parser!(
//   LineTerminator(input: char) -> char {
//     crlf()
//       .or(char('\r'))
//       .or(char('\n'))
//       .parse_stream(input)
//   }

//   Comment(input: char) -> char {
//     char('#')
//       .skip(LineTerminator::new())
//       .parse_stream(input)
//   }
// );



// ok wtf is going on here
// type LineTerminator<I> = Or<Or<CrLf<I>, Token<I>>, Token<I>>;
// pub fn line_terminator<I: Stream<Item = char>>() -> impl Parser<Input=I> {
//     crlf().or(char('\r')).or(char('\n'))
// }


// fn foo<I: Stream<Item = char>>(input: I) -> ParseResult<I::Item, I> {
//   crlf().or(char('\r')).or(char('\n')).parse_stream(input)
// }

// ok wtf is going on here
// type Comment<I> = Skip<Token<I>, LineTerminator<I>>;
// pub fn comment<I: Stream<Item = char>>() -> impl Parser<Input=I>
// {

//   // let lol = line_terminator();

//     char('#')
//         // .skip(many(none_of("".chars())))
//         .skip(LineTerminator::new())
//         // .skip()
// }

// pub fn comma<I: U8Input>(i: I) -> SimpleResult<I,u8>
// {
//   token(i, b',')
// }

// pub fn operation_definition<I: U8Input>(i: I) -> SimpleResult<I,Definition>
// {
//   parse!{i;
//     let op_type = operation_type();
//     white_space() <|> line_terminator();
//     let name = option(|i| name(i).map(Some), None);

//     ret {
//       let op = Operation::new(op_type, name, Vec::new(), Vec::new());
//       Definition::OperationDefinition(op)
//     }
//   }
// }

// pub fn operation_type<I: U8Input>(i: I) -> SimpleResult<I,OperationType>
// {
//   let op_type = |i,b,r| string(i,b).map(|_| r);
//   parse!{i;
//       op_type(b"query", OperationType::Query) <|>
//       op_type(b"mutation", OperationType::Mutation)
//   }
// }

// pub fn name<I: U8Input>(i: I) -> SimpleResult<I,Name>
// {
//   parse!{i;
//     let start = satisfy(|c| is_alpha(c) || c == b'_');
//     let rest = take_while(|c| is_alphanumeric(c) || c == b'_');

//     ret {
//       let mut start = String::from_utf8(vec![start]).unwrap();
//       let rest = String::from_utf8(rest.to_vec()).unwrap();
//       start.push_str(&rest);
//       start
//     }
//   }
// }

// pub fn alias<I: U8Input>(i: I) -> SimpleResult<I, Option<Name>>
// {
//   let parser = |i: I| {
//       parse!{i;
//         let name = name();

//         either(white_space, line_terminator);
//         token(b':');

//         ret Some(name)
//       }
//   };

//   option(i, parser, None)
// }

#[cfg(test)]
mod tests {
    use super::*;
    use combine::{Parser};

    #[test]
    fn test_parse_comment() {
      assert_eq!(Comment::new().parse("#\r\n").map(|x| x.0), Ok('#'));
    }

    #[test]
    fn test_operation_type() {}

    #[test]
    fn test_alias() {}
}
