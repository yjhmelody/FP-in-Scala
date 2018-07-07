package com.yjhmelody.fp.ch2

import com.yjhmelody.fp.ch2.Ch2._
import org.scalatest.FunSuite

class Ch2Suite extends FunSuite {
  test("2.1") {
    assert(fib(0) == 0)
    assert(fib(1) == 1)
    assert(fib(2) == 1)
    assert(fib(3) == 2)
    assert(fib(4) == 3)
    assert(fib(5) == 5)
    assert(fib(6) == 8)
  }

  test("2.2") {
    val as = Array(1, 3, 5)
    assert(isSorted(as, (prev: Int, cur: Int) => prev < cur) == true)
    assert(isSorted(as, (prev: Int, cur: Int) => prev > cur) == false)
  }

  test("2.3 2.4") {
    val curryConcat = curry((x: Int, xs: List[Int]) => x :: xs)
    val concat = uncurry(curryConcat)
    assert(concat(1, List(2, 3)) == List(1,2,3))
    assert(curryConcat(1)(List(2, 3)) == List(1,2,3))
  }

  test("2.5") {
    val as = Array(1, 2, 3)
    val getHeadAndPlusTen = compose(plusTen, getHead)
    assert(getHeadAndPlusTen(as) == as(0) + 10)
  }
}
