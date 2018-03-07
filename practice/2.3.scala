def curry[A, B, C](f:(A, B) => C): A => (B => C) = {
    a: A => (b: B) => f(a, b)
}
// NB: The Function2 trait has a curried method already, so if you wanted to cheat a little you could
// write the answer as f.curried


val lazyConcat = curry((x: Int, xs: List[Int]) => x::xs)

println(lazyConcat(1))
println(lazyConcat(1)(List(2,3)))