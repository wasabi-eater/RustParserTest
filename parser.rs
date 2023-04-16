use core::marker::PhantomData;
use std::cmp::PartialEq;
trait Parser<S, A, E>{
    fn call(self, state: S) -> (S, Result<A, E>);
}
struct Parse();
impl Parse {
    fn ok<S, A, E>(a: A) -> impl Parser<S, A, E> {
        struct New<A>(A);
        impl<S, A, E> Parser<S, A, E> for New<A> {
            fn call(self, state: S) -> (S, Result<A, E>) {
                (state, Ok(self.0))
            }
        }
        New(a)
    }
    fn error<S, A, E>(e: E) -> impl Parser<S, A, E> {
        struct Error<E>(E);
        impl<S, A, E> Parser<S, A, E> for Error<E> {
            fn call(self, state: S) -> (S, Result<A, E>) {
                (state, Err(self.0))
            }
        }
        Error(e)
    }
    
    fn map<S, A, B, E>(this: impl Parser<S, A, E>, f: impl FnOnce(A) -> B) -> impl Parser<S, B, E> {
        struct Map<P, F, A, B>(P, F, PhantomData<Box<dyn FnOnce(A) -> B>>);
        impl<P: Parser<S, A, E>, F: FnOnce(A) -> B, S, A, B, E> Parser<S, B, E> for Map<P, F, A, B> {
            fn call(self, state: S) -> (S, Result<B, E>) {
                let (state, a) = self.0.call(state);
                (state, a.map(self.1))
            }
        }
        Map(this, f, PhantomData)
    }
    
    fn flat_map<S, A, B, E, P: Parser<S, B, E>>(this: impl Parser<S, A, E>, f: impl FnOnce(A) -> P) -> impl Parser<S, B, E> {
        struct FlatMap<P, F, A, P2>(P, F, PhantomData<Box<dyn FnOnce(A) -> P2>>);
        impl<P: Parser<S, A, E>, P2: Parser<S, B, E>, F: FnOnce(A) -> P2, S, A, B, E> Parser<S, B, E> for FlatMap<P, F, A, P2> {
            fn call(self, state: S) -> (S, Result<B, E>) {
                let (state, a) = self.0.call(state);
                let a = match a {
                    Ok(a) => a,
                    Err(e) => return (state, Err(e))
                };
                self.1(a).call(state)
            }
        }
        FlatMap(this, f, PhantomData)
    }
    fn expect<'a, A: PartialEq, E>(item: A, error: E) -> impl Parser<&'a [A], A, E> {
        struct Expect<A, E>(A, E);
        impl<'a, A: PartialEq, E> Parser<&'a [A], A, E> for Expect<A, E> {
            fn call(self, state: &'a [A]) -> (&'a [A], Result<A, E>) {
                if state.len() == 0 || &self.0 != &state[0] {
                    (state, Err(self.1))
                }
                else {
                    (&state[1..], Ok(self.0))
                }
            }
        }
        Expect(item, error)
    }
}
macro_rules! parser {
    {$e: expr} => ($e);
    {let $var: pat = $e1: expr; $e2: expr} => (Parse::flat_map($e1, move |$var| parser!{$e2}));
    {let $var: pat = $e1: expr; $(let $v: pat = $e2: expr);* ; $e3: expr} => (Parse::flat_map($e1, move |$var| parser!{$(let $v = $e2);* ; $e3}));
}
fn create_parser<'a>() -> impl Parser<&'a [char], (), &'static str> {
    parser!{
        let _ = Parse::expect('H', "Not Mached Error");
        let _ = Parse::expect('e', "Not Mached Error");
        let _ = Parse::expect('y', "Not Mached Error");
        Parse::ok(())
    }
}
fn main() {
    let parser = create_parser();
    let code: Vec<_> = "Hey".chars().collect();
    println!("{:?}", parser.call(&code));
    
    let parser = create_parser();
    let code: Vec<_> = "Hi".chars().collect();
    println!("{:?}", parser.call(&code));
}
