def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(prev: Int, cur: Int):Boolean = {
        if (cur == as.length) true
        else if (ordered(as(prev), as(cur))) loop(cur, cur+1)
        else false
    }
    
    loop(0, 1);
}

val as: Array[Int] = Array[Int](1,3,5)

println(isSorted(as, (prev: Int, cur: Int) => prev < cur))
println(isSorted(as, (prev: Int, cur: Int) => prev > cur))
