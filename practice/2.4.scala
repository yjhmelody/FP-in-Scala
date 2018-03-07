def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
}

def curry[A, B, C](f:(A, B) => C): A => (B => C) = {
    A => (b: B) => f(A, b)
}

val lazyConcat = curry((x: Int, xs: List[Int]) => x::xs)

println(lazyConcat(1))
println(lazyConcat(1)(List(2,3)))

val concat = uncurry(lazyConcat)

println(concat(1, List(2,3)))