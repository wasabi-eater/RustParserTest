use core::ops::Deref;
use core::fmt::Formatter;
use core::fmt::Debug;
use core::ops::Range;
use std::rc::Rc;
use std::ops::*;
/// # Parser
/// StateモナドとResultモナドを組み合わせたもので、失敗するかも知れないパース処理を表します
/// `S`: 状態の型
/// `T`: 値の型
/// `E`: エラーの型
pub struct Parser<S, T, E>(Box<dyn FnOnce(S) -> (S, Result<T, E>)>);

/// `parser`マクロは、Haskellのdo式のように、モナドを簡潔に表現する為に用います。
#[macro_export]
macro_rules! parser {
    {let! $var: pat = $e1: expr; $(let! $v: pat = $e2: expr; )* $e3: expr} => ($e1.and_then(move |$var| parser!{$(let! $v = $e2; )* $e3}));
    {$e: expr} => ($e);
}

impl<S: 'static, T: 'static, E: 'static> Parser<S, T, E>{
    /// パーサーを生成します。
    pub fn new(func: impl 'static + FnOnce(S) -> (S, Result<T, E>)) -> Self {
        Self(Box::new(func))
    }
    /// パース処理を実行します。
    pub fn call(self, state: S) -> (S, Result<T, E>) {
        self.0(state)
    }
    /// 処理が成功することを表すパーサーを生成します。
    pub fn ok(value: T) -> Parser<S, T, E>{
        Parser::new(|state| (state, Ok(value)))
    }
    
    /// 処理が失敗することを表すパーサーを生成します。
    pub fn err(e: E) -> Parser<S, T, E> {
        Parser::new(|state: S| (state, Err(e)))
    }
    
    /// パーサーの内部の値を`Result<A, E>`によって包みます。
    pub fn try_parse(self) -> Parser<S, Result<T, E>, E> {
        Parser::new(|state| {
            let (state, result) = self.call(state);
            (state, Ok(result))
        })
    }

    /// 0回以上繰り返しパースします。
    pub fn many(parser: impl 'static + Fn() -> Parser<S, T, E>) -> Parser<S, Vec<T>, E> {
        fn inner<S: 'static, A: 'static, E: 'static>(parser: impl 'static + Fn() -> Parser<S, A, E>, mut vec: Vec<A>) -> Parser<S, Vec<A>, E> {
            parser! {
                let! result = Parser::try_parse(parser());
                (match result{
                    Ok(value) => inner(parser, {vec.push(value); vec}),
                    Err(_) => Parser::ok(vec)
                })
            }
        }
        inner(parser, vec![])
    }

    /// 指定回数繰り返しパースします。
    pub fn n_times(i: usize, parser: impl 'static + Fn() -> Parser<S, T, E>) -> Parser<S, Vec<T>, E> {
        fn inner<S: 'static, T: 'static, E: 'static>(i: usize, parser: impl 'static + Fn() -> Parser<S, T, E>, mut vec: Vec<T>) -> Parser<S, Vec<T>, E> {
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
    
    /// 指定回数以上繰り返しパースします。
    pub fn more_than(i: usize, parser: impl 'static + Fn() -> Parser<S, T, E>) -> Parser<S, Vec<T>, E> {
        fn inner<S: 'static, A: 'static, E: 'static>(i: usize, parser: impl 'static + Fn() -> Parser<S, A, E>, mut vec: Vec<A>) -> Parser<S, Vec<A>, E> {
            parser! {
                let! result = Parser::try_parse(parser());
                (match result{
                    Ok(value) => inner(if i == 0 {0} else {i - 1}, parser, {vec.push(value); vec}),
                    Err(err) =>
                        if i == 0 {
                            Parser::ok(vec)
                        } else {
                            Parser(Box::new(|state| (state, Err(err))))
                        }
                })
            }
        }
        inner(i, parser, vec![])
    }
    /// 前の引数から順にパースを試みます。
    pub fn any(parsers: impl Iterator<Item = Parser<S, T, E>>) -> Parser<S, T, E> {
        parsers.reduce(|x, y| (x | y)).expect("Parser::anyの引数は長さが1以上のイテレーターが必要です")
    }

    pub fn and_then<T2: 'static>(self, f: impl 'static + FnOnce(T) -> Parser<S, T2, E>) -> Parser<S, T2, E> {
        Parser::new(|state|{
            let (state, result) = self.call(state);
            match result {
                Ok(value) => f(value).call(state),
                Err(err) => (state, Err(err))
            }
        })
    }
    pub fn map<B: 'static>(self, f: impl 'static + FnOnce(T) -> B) -> Parser<S, B, E> {
        Parser::new(|state|{
            let (state, result) = self.call(state);
            (state, result.map(f))
        })
    }
}
impl<S: 'static + Copy, E: 'static> Parser<S, S, E>{
    /// 状態を読み取ります。
    pub fn read() -> Self {
        Parser::new(|state| (state, Ok(state)))
    }
}
impl<S: 'static, E: 'static> Parser<S, S, E> {
    /// 状態を書き込みます。
    pub fn write(new_state: S) -> Self {
        Parser::new(|state| (new_state, Ok(state)))
    }
}
impl <S: 'static, T: 'static, E: 'static> BitOr for Parser<S, T, E> {
    type Output =  Self;
    /// 前の引数から順にパースを試みます。
    fn bitor(self, right: Self) -> Self {
        parser! {
            let! left = self.try_parse();
            (match left {
                Ok(value) => Parser::ok(value),
                Err(_) => right
            })
        }
    }
}
impl <T: Clone + 'static, E: 'static> Parser<RcSlice<T>, T, E>{
    /// 条件に合うトークンを一つ読み取る
    pub fn expect_if(cond: impl 'static + FnOnce(&T) -> bool, error: E) -> Self {
        Self::new(|state| {
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
        })
    }
}

impl <T: Clone + PartialEq + 'static, E: 'static> Parser<RcSlice<T>, T, E>{
    /// 条件に合うトークンを一つ読み取る
    pub fn expect(token: T, error: E) -> Self {
        Self::expect_if(move |v: &T| *v == token, error)
    }
}
#[derive(Clone)]
pub struct RcSlice<T> {
    slice: Rc<[T]>,
    offset: usize,
    length: usize
}
impl<T: Debug> Debug for RcSlice<T>{
    fn fmt(&self, formatter: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        self.as_slice().fmt(formatter)
    }
}
impl<T: PartialEq> PartialEq for RcSlice<T> {
    fn eq(&self, right: &Self) -> bool {
        self.as_slice() == right.as_slice()
    }
}
impl<T> RcSlice<T> {
    pub fn as_slice(&self) -> &[T] {
        &self.slice[self.offset .. self.offset + self.length]
    }
    pub fn slice(self, range: Range<usize>) -> Self {
        let Range{start, end} = range;
        Self{slice: self.slice, offset: self.offset + start, length: end - start}
    }
}
impl<'a, T: 'static + Clone> From<&'a [T]> for RcSlice<T> {
    fn from(slice: &[T]) -> Self {
        Self {slice: slice.into(), offset: 0, length: slice.len()}
    }
}
impl<T> Deref for RcSlice<T> {
    type Target = [T];
    fn deref(&self) -> &[T] {
        self.as_slice()
    }
}
fn main(){

}

fn check<T, E>(parser: Parser<RcSlice<char>, T, E>, program: &str, result: Result<T, E>)
    where T: 'static + PartialEq + Debug, E: 'static + PartialEq + Debug{
    let state: RcSlice<char> = program.chars().collect::<Vec<_>>().as_slice().into();
    assert_eq!(parser.call(state).1, result);
}
#[test]
fn expect_test(){
    fn create_parser() -> Parser<RcSlice<char>, (), &'static str> {
        parser! {
            let! _ = Parser::expect('1', "1が必要です");
            let! _ = Parser::expect('2', "2が必要です");
            let! _ = Parser::expect('3', "3が必要です");
            Parser::ok(())
        }
    }
    check(create_parser(), "123A", Ok(()));
    check(create_parser(), "ABC", Err("1が必要です"));
    check(create_parser(), "15", Err("2が必要です"));
    check(create_parser(), "", Err("1が必要です"));
    check(create_parser(), "12", Err("3が必要です"));
}
#[test]
fn or_test() {
    fn create_parser() -> Parser<RcSlice<char>, String, &'static str> {
        let msg = "1または2または3が必要です";
        parser! {
            let! x = Parser::expect('1', msg) | Parser::expect('2', msg) | Parser::expect('3', msg);
            let! y = Parser::any(vec![Parser::expect('1', msg), Parser::expect('2', msg), Parser::expect('3', msg)].into_iter());
            Parser::ok(format!("{}{}", x, y))
        }
    }
    check(create_parser(), "12", Ok("12".to_string()));
    check(create_parser(), "aa", Err("1または2または3が必要です"));
    check(create_parser(), "1b", Err("1または2または3が必要です"));
}
#[test]
fn more_than_test(){
    fn create_parser() -> Parser<RcSlice<char>, String, &'static str> {
        fn digit() -> Parser<RcSlice<char>, char, &'static str> {
            Parser::expect_if(|x| '0' <= *x && *x <= '9', "数字が必要です")
        }
        parser!{
            let! l = Parser::more_than(1, digit);
            let! _ = Parser::expect('.', "小数点が必要です");
            let! r = Parser::more_than(1, digit);
            Parser::ok(format!("{}.{}", 
                l.into_iter().fold(String::new(), |x, y| x + &y.to_string()),
                r.into_iter().fold(String::new(), |x, y| x + &y.to_string())))
        }
    }
    check(create_parser(), "23.4", Ok("23.4".to_string()));
    check(create_parser(), "2.545", Ok("2.545".to_string()));
    check(create_parser(), "ABC", Err("数字が必要です"));
    check(create_parser(), "123", Err("小数点が必要です"));
    check(create_parser(), "", Err("数字が必要です"));
}
#[test]
fn n_times_test(){
    fn create_parser() -> Parser<RcSlice<char>, Vec<char>, &'static str> {
        Parser::n_times(4, || Parser::expect('A', "Aが必要です"))
    }
    check(create_parser(), "AAAAB", Ok(vec!['A', 'A', 'A', 'A']));
    check(create_parser(), "AAB", Err("Aが必要です"));
    check(create_parser(), "", Err("Aが必要です"));
}
#[test]
fn many_test(){
    
    fn create_parser() -> Parser<RcSlice<char>, Vec<char>, &'static str> {
        Parser::many(|| Parser::expect('A', "Aが必要です"))
    }
    check(create_parser(), "AAAAB", Ok(vec!['A', 'A', 'A', 'A']));
    check(create_parser(), "AAB", Ok(vec!['A', 'A']));
    check(create_parser(), "", Ok(vec![]));
}
#[test]
fn try_parse_test() {
    fn create_parser() -> Parser<RcSlice<char>, Result<char, &'static str>, &'static str> {
        Parser::expect('A', "Aが必要です").try_parse()
    }
    check(create_parser(), "A", Ok(Ok('A')));
    check(create_parser(), "B", Ok(Err("Aが必要です")))
}
