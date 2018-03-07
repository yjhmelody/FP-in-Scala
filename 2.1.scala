def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, prev: Int, cur: Int): Int =
        if (n == 0) prev
        else loop(n-1, cur, prev + cur)
    loop(n, 0, 1)
}

println(fib(0))
println(fib(1))
println(fib(2))
println(fib(3))
println(fib(4))
println(fib(5))
println(fib(6))
println(fib(7))
println(fib(8))
println(fib(9))