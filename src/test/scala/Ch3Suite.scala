package com.yjhmelody.fp.ch3

import com.yjhmelody.fp.ch3._
import org.scalatest.FunSuite

class Ch3Suite extends FunSuite {
  val ex1: MyList[Double] = MyNil
  val ex2: MyList[Int] = Cons(1, MyNil)
  val ex3: MyList[String] = Cons("a", Cons("b", MyNil))
  var ex4 = MyList(1, 2, 3, 4, 5)

  test("match 3.1") {
    val x = ex4 match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case MyNil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => 3
      case Cons(h, t) => h + MyList.sum(t)
      case _ => 101
    }
    assert(x == 3)
  }

  test("tail 3.2") {
    assert(MyList.tail(ex1) == MyNil)
    assert(MyList.tail(ex2) == MyNil)
    assert(MyList.tail(ex3) == Cons("b", MyNil))
    assert(MyList.tail(ex4) == MyList(2, 3, 4, 5))
  }

  test("setHead 3.3") {
    assert(MyList.setHead(ex1, 1.0) == Cons(1.0, MyNil))
    assert(MyList.setHead(ex2, 2) == MyList(2))
    assert(MyList.setHead(ex3, "c") == MyList("c", "b"))
    assert(MyList.setHead(ex4, 0) == MyList(0, 2, 3, 4, 5))
  }

  test("drop 3.3") {
    assert(MyList.drop(ex1, 1) == MyNil)
    assert(MyList.drop(ex2, 2) == MyNil)
    assert(MyList.drop(ex3, 3) == MyNil)
    assert(MyList.drop(ex4, 4) == MyList(5))
  }


  test("dropWhile 3.5") {
    assert(MyList.dropWhile(ex1, (x: Double) => x == 0) == MyNil)
    assert(MyList.dropWhile(ex2, (x: Int) => x == 1) == MyNil)
    assert(MyList.dropWhile(ex3, (x: String) => x == "b") == MyList("a", "b"))
    assert(MyList.dropWhile(ex4, (x: Int) => x == 3) == MyList(1, 2, 3, 4, 5))
  }

  test("init 3.6") {
    assert(MyList.init(ex1) == MyNil)
    assert(MyList.init(ex2) == MyNil)
    assert(MyList.init(ex3) == MyList("a"))
    assert(MyList.init(ex4) == MyList(1, 2, 3, 4))
    assert(MyList.init2(ex1) == MyNil)
    assert(MyList.init2(ex2) == MyNil)
    assert(MyList.init2(ex3) == MyList("a"))
    assert(MyList.init2(ex4) == MyList(1, 2, 3, 4))
  }

  test("foldRight 3.7") {
    assert(MyList.foldRight(MyList(1, 2, 3, 4), MyNil: MyList[Int])(Cons(_, _)) == MyList(1, 2, 3, 4))
  }

  test("length 3.9") {
    assert(MyList.length(ex1) == 0)
    assert(MyList.length(ex2) == 1)
    assert(MyList.length(ex3) == 2)
    assert(MyList.length(ex4) == 5)
  }

  test("foldLeft 3.10") {
    assert(MyList.foldLeft(MyList(1, 2, 3, 4), 0)(_ + _) == 10)
  }

  test("sum3 product3 3.11") {
    assert(MyList.sum3(ex4) == 15)
    assert(MyList.product3(ex1) == 1)
    assert(MyList.product3(MyList[Double](1, 2, 3, 4)) == 24)
  }

  test("reverse 3.12") {
    assert(MyList.reverse(ex4) == MyList(5, 4, 3, 2, 1))
  }

  test("append 3.14") {
    assert(MyList.append(ex4, MyList(6, 7)) == MyList(1, 2, 3, 4, 5, 6, 7))
  }

  test("concat 3.15") {
    assert(MyList.concat(MyList[MyList[Int]](MyList(1, 2, 3), MyList(4, 5, 6))) == MyList(1, 2, 3, 4, 5, 6))
  }

  test("plusOne add1 3.16") {
    assert(MyList.plusOne(ex4) == MyList(2, 3, 4, 5, 6))
    assert(MyList.add1(ex4) == MyList(2, 3, 4, 5, 6))
  }

  test("doubleToString 3.17") {
    assert(MyList.doubleToString(MyList[Double](1.1, 2.2, 3.3, 4.4)) == MyList("1.1", "2.2", "3.3", "4.4"))
    assert(MyList.doubleToString2(MyList[Double](2.2, 10.00)) == MyList("2.2", "10.0"))
  }

  test("map 3.18") {
    assert(MyList.map(ex4)(_ * 2) == MyList(2, 4, 6, 8, 10))
  }

  test("filter 3.19") {
    assert(MyList.filter(ex4)(x => x != 4) == MyList(1, 2, 3, 5))
  }

  test("flatMap 3.20") {
    assert(MyList.flatMap(MyList(1, 2, 3))(i => MyList(i, i)) == MyList(1, 1, 2, 2, 3, 3))
  }

  test("filterViaFlatMap 3.21") {
    assert(MyList.filterViaFlatMap(ex4)(x => x != 4) == MyList(1, 2, 3, 5))
  }

  test("3.22") {
    assert(MyList.plus(ex4, ex4)(_ + _) == MyList(2, 4, 6, 8, 10))
  }

  test("3.23") {
    assert(MyList.zipWith(ex4, ex4)(_ * _) == MyList(1, 4, 9, 16, 25))
  }

  test("3.24") {
    assert(MyList.hasSubsequence(MyList(1, 2, 3), MyList(1, 2)))
  }

  test("3.25-3.29") {
    val tree = Branch(
      Branch(
        Leaf(1),
        Leaf(2),
      ),
      Branch(
        Leaf(3),
        Leaf(4)
      )
    )
    val res = Branch(
      Branch(
        Leaf(2),
        Leaf(4)
      ),
      Branch(
        Leaf(6),
        Leaf(8)
      )
    )
    assert(Tree.size(tree) == 7)
    assert(Tree.maximun(tree) == 4)
    assert(Tree.depth(tree) == 2)
    assert(Tree.map(tree)(_ * 2) == res)

    assert(Tree.fold(tree)(x => Leaf(x * 2): Tree[Int])(Branch(_, _)) == res)

    assert(Tree.sizeViaFold(tree) == 7)
    assert(Tree.maximunViaFold(tree) == 4)
    assert(Tree.depthViaFold(tree) == 2)
    assert(Tree.mapViaFold(tree)(_ * 2) == res)

  }
}
