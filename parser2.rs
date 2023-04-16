type Parser<S, A, E> = Box<dyn FnOnce(S) -> (S, Result<A, E>)>;
#[macro_export]
macro_rules! parser {
    {let! $var: pat = $e1: expr; $(let! $v: pat = $e2: expr);* ; $e3: expr} => (Box::new(|state|{
        let (state, result) = $e1(state);
        match result {
            Ok($var) => parser!{$(let! $v = $e2);* ; $e3}(state),
            Err(e) => (state, Err(e))
        }
    }) as Parser<_, _, _>);
    {let! $var: pat = $e1: expr; $e2: expr} => (Box::new(|state|{
        let (state, result) = $e1(state);
        match result {
            Ok($var) => $e2(state),
            Err(e) => (state, Err(e)),
        }
    }) as Parser<_, _, _>);
    {$e: expr} => ($e);
}
pub fn ret<S, A: 'static, E: 'static>(value: A) -> Parser<S, A, E>{
    Box::new(|state| (state, Ok(value)))
}
pub fn get<S: 'static + Copy, E: 'static>() -> Parser<S, S, E> {
    Box::new(|state| (state, Ok(state)))
}
pub fn set<S: 'static, E: 'static>(new_state: S) -> Parser<S, S, E> {
    Box::new(|state| (new_state, Ok(state)))
}
pub fn try_parse<S: 'static, A: 'static, E: 'static>(parser: Parser<S, A, E>) -> Parser<S, Result<A, E>, E> {
    Box::new(|state| {
        let (state, result) = parser(state);
        (state, Ok(result))
    })
}
pub fn expect<S, E: 'static>(cond: impl 'static + FnOnce(&S) -> bool, error: E) -> Parser<Vec<S>, S, E> {
    Box::new(|mut state| {
        match state.pop() {
            None => (state, Err(error)),
            Some(last) =>
                if cond(&last) {
                    (state, Ok(last))
                } else {
                    state.push(last);
                    (state, Err(error))
                }
        }
    })
}
fn create_parser() -> Parser<Vec<char>, (), &'static str> {
    parser!{
        let! _ = expect(|x| *x == 'H', "Not Matched Error");
        let! _ = expect(|x| *x == 'E', "Not Matched Error");
        let! _ = expect(|x| *x == 'Y', "Not Matched Error");
        ret(())
    }
}
fn main() {
    let vec: Vec<_> = "HEY".chars().rev().collect();
    assert_eq!(create_parser()(vec), (vec![], Ok(())));
    
    let vec: Vec<_> = "Hi".chars().rev().collect();
    assert_eq!(create_parser()(vec), (vec!['i'], Err("Not Matched Error")));
}
