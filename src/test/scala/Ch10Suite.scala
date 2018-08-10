package com.yjhmelody.fp.ch10

import org.scalatest.FunSuite

class Ch10Suite extends FunSuite {
  val stringMonoid = new Monoid[String] {
    override def op(a1: String, a2: String) = a1 + a2

    override def zero(): String = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    override def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2

    override def zero(): List[A] = Nil
  }

  test("monoid trait") {
    assert(stringMonoid.op("a", "b") == "ab")
    assert(stringMonoid.zero() == "")

    assert(listMonoid.op(List(1), List(2)) == List(1, 2))
    assert(listMonoid.zero() == Nil)
  }

  test("10.1") {
    val intAddition = new Monoid[Int] {
      override def op(a1: Int, a2: Int): Int = a1 + a2

      override def zero(): Int = 0
    }

    val intMultiplication = new Monoid[Int] {
      override def op(a1: Int, a2: Int): Int = a1 * a2

      override def zero(): Int = 1
    }

    val booleanOr = new Monoid[Boolean] {
      override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

      override def zero(): Boolean = false
    }

    val booleanAnd = new Monoid[Boolean] {
      override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

      override def zero(): Boolean = true
    }
  }

  test("10.2") {
    def optionMonoid[A] = new Monoid[Option[A]] {
      override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2

      override def zero(): Option[A] = None
    }

    def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
      override def op(a1: A, a2: A): A = m.op(a2, a1)

      override def zero(): A = m.zero()
    }

    def firstOptionMonoid[A] = optionMonoid[A]

    def lastOptionMonoid[A] = dual(firstOptionMonoid)
  }

  test("10.3") {
    def endoMonoid[A] = new Monoid[A => A] {
      override def op(a1: A => A, a2: A => A): A => A = a1 compose a2

      override def zero(): A => A = a => a
    }
  }
}