use core::fmt::Formatter;
use core::fmt::Debug;
use core::ops::Range;
use core::ops::Index;
use std::rc::Rc;

struct Parser<S, A, E>(Box<dyn FnOnce(S) -> (S, Result<A, E>)>);
#[macro_export]
macro_rules! parser {
    {let! $var: pat = $e1: expr; $(let! $v: pat = $e2: expr);* ; $e3: expr} => ($e1.flat_map(|$var| parser!{$(let! $v = $e2);* ; $e3}));
    {let! $var: pat = $e1: expr; $e2: expr} => ($e1.flat_map(|$var| $e2));
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
    pub fn flat_map<B>(self, f: impl 'static + FnOnce(A) -> Parser<S, B, E>) -> Parser<S, B, E> {
        Parser(Box::new(|state|{
            let (state, result) = self.0(state);
            match result {
                Ok(value) => f(value).0(state),
                Err(err) => (state, Err(err))
            }
        }))
    }
}
impl<S: 'static + Copy, E: 'static> Parser<S, S, E>{
    pub fn get() -> Self {
        Parser(Box::new(|state| (state, Ok(state))))
    }
}
impl<S: 'static, E: 'static> Parser<S, S, E> {
    pub fn set(new_state: S) -> Self {
        Parser(Box::new(|state| (new_state, Ok(state))))
    }
}
#[derive(Clone)]
pub struct RcQueue<A> {
    slice: Rc<[A]>,
    offset: usize,
    length: usize
}
impl<A: Debug> Debug for RcQueue<A>{
    fn fmt(&self, formatter: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        self.as_slice().fmt(formatter)
    }
}
impl<A: PartialEq> PartialEq for RcQueue<A> {
    fn eq(&self, right: &Self) -> bool {
        self.as_slice() == right.as_slice()
    }
}
impl<A> RcQueue<A> {
    pub fn as_slice(&self) -> &[A] {
        &self.slice[self.offset..self.offset + self.length]
    }
    pub fn get(&self, ind: usize) -> Option<&A> {
        self.as_slice().get(ind)
    }
    pub fn len(&self) -> usize {
        self.length
    }
    pub fn pop_front(&mut self) -> Option<&A> {
        self.offset += 1;
        if self.length > 0 {
            self.length -= 1;
            Some(&self.slice[self.offset - 1])
        }
        else {
            None
        }
    }
    pub fn pop_back(&mut self) -> Option<&A> {
        if self.length > 0 {
            self.length -= 1;
            Some(&self.slice[self.offset + self.length])
        }
        else {
            None
        }
    }
}
impl <S: Clone, E: 'static> Parser<RcQueue<S>, S, E>{
    pub fn expect(cond: impl 'static + FnOnce(&S) -> bool, error: E) -> Self {
        Self(Box::new(|mut state| {
            let state_ = state.clone();
            match state.pop_front() {
                None => (state, Err(error)),
                Some(last) =>
                    if cond(&last) {
                        let last = last.clone();
                        (state, Ok(last))
                    } else {
                        (state_, Err(error))
                    }
            }
        }))
    }
}
impl<'a, A: 'static + Clone> From<&'a [A]> for RcQueue<A> {
    fn from(slice: &[A]) -> Self {
        Self {slice: slice.into(), offset: 0, length: slice.len()}
    }
}
impl<A> Index<usize> for RcQueue<A> {
    type Output = A;
    fn index(&self, ind: usize) -> &A {
        self.get(ind).expect("Index out of range")
    }
}
impl<A> Index<Range<usize>> for RcQueue<A> {
    type Output = [A];
    fn index(&self, ind: Range<usize>) -> &[A] {
        let Range{ end, .. } = ind;
        if end < self.length {
            &self.as_slice()[ind]
        }
        else {
            panic!("Index out of range")
        }
    }
}

fn create_parser() -> Parser<RcQueue<char>, (), &'static str> {
    parser!{
        let! _ = Parser::expect(|x| *x == 'H', "Not Matched Error");
        let! _ = Parser::many(|| Parser::expect(|x| *x == 'E', "Not Matched Error"));
        let! _ = Parser::expect(|x| *x == 'Y', "Not Matched Error");
        Parser::ret(())
    }
}
fn main() {
    let vec: RcQueue<char> = "HEEEY".chars().collect::<Vec<_>>().as_slice().into();
    assert_eq!(create_parser().0(vec), (RcQueue::from(&[] as &[char]), Ok(())));

    let vec: RcQueue<char> = "Hi".chars().collect::<Vec<_>>().as_slice().into();
    assert_eq!(create_parser().0(vec), (RcQueue::from(&['i'] as &[char]), Err("Not Matched Error")));
}
