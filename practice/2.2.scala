def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(prev: Int, cur: Int):Boolean = {
        if (cur == as.length) true
        else if (ordered(as(prev), as(cur))) loop(cur, cur+1)
        else false
    }
    
    loop(0, 1);

    // @annotation.tailrec
    // def go(n: Int): Boolean =
    //     if (n >= as.length-1) true
    //     else if (gt(as(n), as(n+1))) false
    //     else go(n+1)
    // go(0)

}

// val as: Array[Int] = Array[Int](1,3,5)
val as = Array(1,3,5)

println(isSorted(as, (prev: Int, cur: Int) => prev < cur))
println(isSorted(as, (prev: Int, cur: Int) => prev > cur))
