package com.yjhmelody.fp.ch2

object Ch2 {
  def main(args: Array[String]): Unit = {
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

    val as = Array(1, 3, 5)
    println(isSorted(as, (prev: Int, cur: Int) => prev < cur))
    println(isSorted(as, (prev: Int, cur: Int) => prev > cur))

    // NB: The Function2 trait has a curried method already, so if you wanted to cheat a little you could
    // write the answer as f.curried
    val curryConcat = curry((x: Int, xs: List[Int]) => x :: xs)
    println(curryConcat(1))
    println(curryConcat(1)(List(2, 3)))

    val concat = uncurry(curryConcat)
    println(concat(1, List(2, 3)))

    val as2 = Array(1, 2, 3)
    val getHeadAndPlusTen = compose(plusTen, getHead)
    println(getHeadAndPlusTen(as2))
  }

  // 2.1
  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, prev: Int, cur: Int): Int =
      if (n == 0) prev
      else loop(n - 1, cur, prev + cur)

    loop(n, 0, 1)
  }

  // 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(prev: Int, cur: Int): Boolean = {
      if (cur == as.length) true
      else if (ordered(as(prev), as(cur))) loop(cur, cur + 1)
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

  // 2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a: A => (b: B) => f(a, b)
  }

  // 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  // 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }

  def getHead(as: Array[Int]) = {
    as(0)
  }

  def plusTen(x: Int) = {
    x + 10
  }
}