use core::ops::Deref;
use core::fmt::Formatter;
use core::fmt::Debug;
use core::ops::Range;
use core::ops::Index;
use std::rc::Rc;

pub struct Parser<S, A, E>(Box<dyn FnOnce(S) -> (S, Result<A, E>)>);
#[macro_export]
macro_rules! parser {
    {let! $var: pat = $e1: expr; $(let! $v: pat = $e2: expr; )* $e3: expr} => ($e1.flat_map(move |$var| parser!{$(let! $v = $e2; )* $e3}));
    {$e: expr} => ($e);
}
impl<S: 'static, A: 'static, E: 'static> Parser<S, A, E>{
    pub fn ret(value: A) -> Parser<S, A, E>{
        Parser(Box::new(|state| (state, Ok(value))))
    }
    pub fn try_parse(self) -> Parser<S, Result<A, E>, E> {
        Parser(Box::new(|state| {
            let (state, result) = self.0(state);
            (state, Ok(result))
        }))
    }
    pub fn many(parser: impl 'static + Fn() -> Parser<S, A, E>) -> Parser<S, Vec<A>, E> {
        fn inner<S: 'static, A: 'static, E: 'static>(parser: impl 'static + Fn() -> Parser<S, A, E>, mut vec: Vec<A>) -> Parser<S, Vec<A>, E> {
            parser! {
                let! result = Parser::try_parse(parser());
                (match result{
                    Ok(value) => inner(parser, {vec.push(value); vec}),
                    Err(_) => Parser::ret(vec)
                })
            }
        }
        inner(parser, vec![])
    }
    pub fn n_(i: usize, parser: impl 'static + Fn() -> Parser<S, A, E>) -> Parser<S, Vec<A>, E> {
        fn inner<S: 'static, A: 'static, E: 'static>(i: usize, parser: impl 'static + Fn() -> Parser<S, A, E>, mut vec: Vec<A>) -> Parser<S, Vec<A>, E> {
            if i == 0 { return Parser(Box::new(|state| (state, Ok(vec)))); }
            parser! {
                let! result = Parser::try_parse(parser());
                (match result{
                    Ok(value) => inner(i - 1, parser, {vec.push(value); vec}),
                    Err(err) => Parser(Box::new(|state| (state, Err(err))))
                })
            }
        }
        inner(i, parser, vec![])
    }
    pub fn flat_map<B>(self, f: impl 'static + FnOnce(A) -> Parser<S, B, E>) -> Parser<S, B, E> {
        Parser(Box::new(|state|{
            let (state, result) = self.0(state);
            match result {
                Ok(value) => f(value).0(state),
                Err(err) => (state, Err(err))
            }
        }))
    }
    pub fn map<B>(self, f: impl 'static + FnOnce(A) -> B) -> Parser<S, B, E> {
        Parser(Box::new(|state|{
            let (state, result) = self.0(state);
            (state, result.map(f))
        }))
    }
    pub fn or(self, right: Parser<S, A, E>) -> Parser<S, A, E> {
        parser! {
            let! left = self.try_parse();
            (match left {
                Ok(value) => Parser::ret(value),
                Err(_) => right
            })
        }
    }
    pub fn any(parsers: impl Iterator<Item = Parser<S, A, E>>) -> Parser<S, A, E> {
        parsers.reduce(Parser::or).expect("Parser::anyの引数は長さが1以上のイテレーターが必要です")
    }
    pub fn more_than(i: usize, parser: impl 'static + Fn() -> Parser<S, A, E>) -> Parser<S, Vec<A>, E> {
        fn inner<S: 'static, A: 'static, E: 'static>(i: usize, parser: impl 'static + Fn() -> Parser<S, A, E>, mut vec: Vec<A>) -> Parser<S, Vec<A>, E> {
            parser! {
                let! result = Parser::try_parse(parser());
                (match result{
                    Ok(value) => inner(if i == 0 {0} else {i - 1}, parser, {vec.push(value); vec}),
                    Err(err) =>
                        if i == 0 {
                            Parser::ret(vec)
                        } else {
                            Parser(Box::new(|state| (state, Err(err))))
                        }
                })
            }
        }
        inner(i, parser, vec![])
    }
}
impl<S: 'static + Copy, E: 'static> Parser<S, S, E>{
    pub fn read() -> Self {
        Parser(Box::new(|state| (state, Ok(state))))
    }
}
impl<S: 'static, E: 'static> Parser<S, S, E> {
    pub fn write(new_state: S) -> Self {
        Parser(Box::new(|state| (new_state, Ok(state))))
    }
}
impl <S: Clone, E: 'static> Parser<RcSlice<S>, S, E>{
    pub fn expect(cond: impl 'static + FnOnce(&S) -> bool, error: E) -> Self {
        Self(Box::new(|state| {
            if state.len() == 0 {
                return (state, Err(error));
            }
            let last = &state[0];
            if cond(last) {
                let last = last.clone();
                let len = state.len();
                (state.slice(1..len), Ok(last))
            } else {
                (state, Err(error))
            }
            
        }))
    }
}
#[derive(Clone)]
pub struct RcSlice<A> {
    slice: Rc<[A]>,
    offset: usize,
    length: usize
}
impl<A: Debug> Debug for RcSlice<A>{
    fn fmt(&self, formatter: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        self.as_slice().fmt(formatter)
    }
}
impl<A: PartialEq> PartialEq for RcSlice<A> {
    fn eq(&self, right: &Self) -> bool {
        self.as_slice() == right.as_slice()
    }
}
impl<A> RcSlice<A> {
    pub fn as_slice(&self) -> &[A] {
        &self.slice[self.offset..self.offset + self.length]
    }
    pub fn slice(self, range: Range<usize>) -> Self {
        let Range{start, end} = range;
        Self{slice: self.slice, offset: self.offset + start, length: end - start}
    }
}
impl<'a, A: 'static + Clone> From<&'a [A]> for RcSlice<A> {
    fn from(slice: &[A]) -> Self {
        Self {slice: slice.into(), offset: 0, length: slice.len()}
    }
}
impl<A> Deref for RcSlice<A> {
    type Target = [A];
    fn deref(&self) -> &[A] {
        self.as_slice()
    }
}
fn create_parser() -> Parser<RcSlice<char>, String, String> {
    fn digit() -> Parser<RcSlice<char>, char, String> {
        Parser::expect(|x| '0' <= *x && *x <= '9', "数字が必要です".to_string())
    }
    fn expect_char(c: char) -> Parser<RcSlice<char>, char, String>{
        Parser::expect(move |x| *x == c, c.to_string() + "文字が異なります")
    }
    parser!{
        let! l = Parser::more_than(1, digit);
        let! _ = expect_char('.');
        let! r = Parser::more_than(1, digit);
        {
            Parser::ret(format!("{}.{}", 
                l.into_iter().fold(String::new(), |x, y| x + &y.to_string()),
                r.into_iter().fold(String::new(), |x, y| x + &y.to_string())))
        }
    }
}
fn main() {
    let state: RcSlice<char> = "23.4".chars().collect::<Vec<_>>().as_slice().into();
    println!("{:?}", create_parser().0(state).1);

    let state: RcSlice<char> = "55.4444".chars().collect::<Vec<_>>().as_slice().into();
    println!("{:?}", create_parser().0(state).1);
    
    let state: RcSlice<char> = "ABC".chars().collect::<Vec<_>>().as_slice().into();
    println!("{:?}", create_parser().0(state).1);
}
