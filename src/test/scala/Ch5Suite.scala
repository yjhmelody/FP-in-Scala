package com.yjhmelody.fp.ch5

import org.scalatest.FunSuite


class Ch5Suite extends FunSuite {
  val s1 = Stream[Int](1, 2, 3, 4)
  val s2 = Stream(() => 1, () => 2 * 2, () => 3 * 3)

  test("5.1") {
    assert(s1.toList() == List(1,2,3,4))
    assert(s2.toList()(2)() == 9)
  }

  test("5.2") {
    assert(s1.take(2).toList() == List(1,2))
    assert(s2.take(1).toList()(0)() == 1)

    assert(s1.drop(2).toList() == List(3,4))
    assert(s2.drop(1).toList()(1)() == 9)
  }

  test("5.3") {
    assert(s1.takeWhile(_ == 1).toList() == List(1))
  }
}
