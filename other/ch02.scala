// 在数组中查找元素的多态函数
def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
        if (n >= as.length) -1
        else if (p(as(n))) n
        else loop(n + 1)

    loop(0)
}

val as: Array[Int] = Array[Int](1,3,5)

val x = findFirst(as, (x: Int) => x == 2)
val y = findFirst(as, (x: Int) => x == 5)

println(x)
println(y)