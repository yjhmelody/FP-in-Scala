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

  test("5.4") {
    assert(s1.forAll2(_ < 5) == true)
    assert(s2.forAll2(_ () < 5) == false)
  }

  test("5.5") {
    assert(s1.takeWhile2(_ <= 2).toList() == Stream(1, 2).toList())
    assert(Stream(5, 4, 3, 4, 5).takeWhile2(_ >= 4).toList() == List(5, 4))
  }

  test("5.6") {
    assert(s1.headOption2().get == 1)
    assert(s2.headOption2().get() == 1)
  }

  test("5.7") {
    assert(s1.map(_ * 2).toList() == List(2, 4, 6, 8))
  }
}
