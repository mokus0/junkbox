fn compose<A,B,C>(f: fn@(B)->C, g: fn@(A)->B) -> fn(A) -> C {
    fn@(x: A) -> C {f(g(x))}
}

fn say(&&x: str) {
    io::println(x);
}

fn hello(&&name: str) -> str {
    ret #fmt("hello, %s!", name);
}

fn main() {
    compose(say, hello)("everybody");
}