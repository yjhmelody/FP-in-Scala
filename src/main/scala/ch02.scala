package com.yjhmelody.fp.ch2

object Ch2 {
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

    loop(0, 1)
  }

  // 2.3
  def curry[A, B, C](f: (A, B) => C): A => B => C = {
    a: A => (b: B) => f(a, b)
  }

  // 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  // 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a: A => f(g(a))
  }

  def getHead(as: Array[Int]): Int = {
    as(0)
  }

  def plusTen(x: Int): Int = {
    x + 10
  }
}