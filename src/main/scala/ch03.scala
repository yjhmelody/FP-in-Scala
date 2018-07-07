package com.yjhmelody.fp.ch3


sealed trait MyList[+A]
case object MyNil extends MyList[Nothing]
case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {
  def sum(ints: MyList[Int]): Int = ints match {
    case MyNil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: MyList[Double]): Double = ds match {
    case MyNil => 1.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): MyList[A] =
    if (as.isEmpty) MyNil
    else Cons(as.head, apply(as.tail: _*))

  def append[A](a1: MyList[A], a2: MyList[A]): MyList[A] = a1 match {
    case MyNil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  // 3.2
  def tail[A](xs: MyList[A]): MyList[A] = xs match {
    // case Nil => sys.error("tail of empty MyList")
    case MyNil => MyNil
    case Cons(_, xs) => xs
  }

  // 3.3
  def setHead[A](xs: MyList[A], x: A): MyList[A] = xs match {
    // case Nil => sys.error("tail of empty MyList")
    case MyNil => Cons(x, MyNil)
    case Cons(_, xs) => Cons(x, xs)
  }

  // 3.4
  @annotation.tailrec
  def drop[A](xs: MyList[A], n: Int): MyList[A] =
    if (n <= 0) xs
    else xs match {
      case MyNil => MyNil
      case Cons(_, t) => drop(t, n - 1)
    }

  // 3.5
  @annotation.tailrec
  def dropWhile[A](xs: MyList[A], f: A => Boolean): MyList[A] = xs match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => xs
  }

  // 3.6
  def init[A](xs: MyList[A]): MyList[A] = xs match {
    // case Nil => sys.error("init of empty MyList")
    case MyNil => MyNil
    case Cons(_, MyNil) => MyNil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def init2[A](xs: MyList[A]): MyList[A] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]

    @annotation.tailrec
    def go(cur: MyList[A]): MyList[A] = cur match {
      // case Nil => sys.error("init of empty MyList")
      case MyNil => MyNil
      case Cons(_, MyNil) => MyList(buf.toList: _*)
      case Cons(h, t) => buf += h; go(t)
    }

    go(xs)
  }

  // example 3.2
  def foldRight[A, B](as: MyList[A], z: B)(f: (A, B) => B): B = as match {
    case MyNil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(xs: MyList[Int]) =
    foldRight(xs, 0)(_ + _)

  def product2(xs: MyList[Double]) =
    foldRight(xs, 1.0)(_ * _)

  // 3.9
  def length[A](xs: MyList[A]): Int =
    foldRight(xs, 0)((_, acc) => acc + 1)

  // 3.10
  @annotation.tailrec
  def foldLeft[A, B](as: MyList[A], z: B)(f: (B, A) => B): B = as match {
    case MyNil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  // 3.11
  def sum3(xs: MyList[Int]) = foldLeft(xs, 0)(_ + _)

  def product3(xs: MyList[Double]) = foldLeft(xs, 1.0)(_ * _)

  def length2[A](xs: MyList[A]): Int = foldLeft(xs, 0)((acc, _) => acc + 1)

  // 3.12
  def reverse[A](xs: MyList[A]): MyList[A] =
    foldLeft(xs, MyList[A]())((acc, h) => Cons(h, acc))

  // 3.13
  def foldRightViaFoldLeft[A, B](l: MyList[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((b, a) => f(a, b))

  def foldRightViaFoldLeft_1[A, B](l: MyList[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

  def foldLeftViaFoldRight[A, B](l: MyList[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  // 3.14
  def appendViaFoldRight[A](l: MyList[A], r: MyList[A]): MyList[A] =
    foldRight(l, r)(Cons(_, _))

  // 3.15
  def concat[A](xs: MyList[MyList[A]]): MyList[A] =
    foldRight(xs, MyNil: MyList[A])(append)

  // 3.16
  def plusOne(xs: MyList[Int]): MyList[Int] = xs match {
    case MyNil => MyNil
    case Cons(x, xs) => Cons(x + 1, plusOne(xs))
  }

  // 3.16-2
  def add1(xs: MyList[Int]): MyList[Int] =
    foldRight(xs, MyNil: MyList[Int])((h, t) => Cons(h + 1, t))

  // 3.17
  def doubleToString(xs: MyList[Double]): MyList[String] = xs match {
    case MyNil => MyNil
    case Cons(x, xs) => Cons(x.toString(), doubleToString(xs))
  }

  // 3.17-2
  def doubleToString2(xs: MyList[Double]): MyList[String] =
    foldRight(xs, MyNil: MyList[String])((h, t) => Cons(h.toString, t))

  // 3.18*

  //     A natural solution is using foldRight, but our implementation of foldRight is not stack-safe. We
  // can use foldRightViaFoldLeft to avoid the stack overflow (variation 1), but more commonly, with
  // our current implementation of MyList, map will just be implemented using local mutation (variation
  // 2). Again, note that the mutation isn’t observable outside the function, since we’re only mutating a
  // buffer that we’ve allocated.

  def map[A, B](xs: MyList[A])(f: A => B): MyList[B] = xs match {
    case MyNil => MyNil
    case Cons(x, xs) => Cons(f(x), map(xs)(f))
  }

  // 3.19
  def filter[A](xs: MyList[A])(f: A => Boolean): MyList[A] = {
    def loop(xs: MyList[A], prev: A): MyList[A] = {
      xs match {
        case MyNil => MyNil
        case Cons(x, xs) if f(x) => Cons(x, loop(xs, x))
        case Cons(x, xs) => loop(xs, x)
      }
    }

    xs match {
      case MyNil => MyNil
      case Cons(x, xs) => loop(Cons(x, xs), x)
    }
  }

  def filter2[A](xs: MyList[A])(f: A => Boolean): MyList[A] =
    foldRight(xs, MyNil: MyList[A])((h, t) => if (f(h)) Cons(h, t) else t)

  def filter3[A](xs: MyList[A])(f: A => Boolean): MyList[A] = {
    val buf = new collection.mutable.ListBuffer[A]

    def loop(xs: MyList[A]): Unit = xs match {
      case MyNil => ()
      case Cons(h, t) => if (f(h)) buf += h; loop(t)
    }

    loop(xs)
    MyList(buf.toList: _*)
    // converting from the standard Scala MyList to the MyList we've defined here
  }

  // 3.20
  // This could also be implemented directly using `foldRight`.
  def flatMap[A, B](xs: MyList[A])(f: A => MyList[B]): MyList[B] = xs match {
    case MyNil => MyNil
    case Cons(x, xs) => append(f(x), flatMap(xs)(f))
  }

  def flatMap2[A, B](xs: MyList[A])(f: A => MyList[B]): MyList[B] =
    concat(map(xs)(f))

  // hard
  // def flatMap3[A, B](xs: MyList[A])(f: A => MyList[B]): MyList[B] =
  // foldRight(xs, Nil:MyList[B])((x, xs) => Cons(f(x), xs))

  // 3.21
  // Regrad inner MyList as a container. If true return MyList(x) else Nil, and Nil will be droped by flatMap.
  def filterViaFlatMap[A](xs: MyList[A])(f: A => Boolean): MyList[A] =
    flatMap(xs)(x => if (f(x)) MyList(x) else MyNil)

  // 3.22
  //     To match on multiple values, we can put the values into a pair and match on the pair, as shown next,
  // and the same syntax extends to matching on N values (see sidebar “Pairs and tuples in Scala” for more
  // about pair and tuple objects). You can also (somewhat less conveniently, but a bit more efficiently)
  // nest pattern matches: on the right hand side of the =>, simply begin another match expression. The
  // inner match will have access to all the variables introduced in the outer match.
  // mine
  def plus[A](xs: MyList[A], ys: MyList[A])(f: (A, A) => A): MyList[A] = (xs, ys) match {
    case (MyNil, _) => MyNil
    case (_, MyNil) => MyNil
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), plus(xs, ys)(f))
  }

  def addPairwise(a: MyList[Int], b: MyList[Int]): MyList[Int] = (a, b) match {
    case (MyNil, _) => MyNil
    case (_, MyNil) => MyNil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))
  }

  // 3.23
  def zipWith[A, B, C](xs: MyList[A], ys: MyList[B])(f: (A, B) => C): MyList[C] = (xs, ys) match {
    case (MyNil, _) => MyNil
    case (_, MyNil) => MyNil
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
  }

  // 3.24
  def hasSubsequence[A](sup: MyList[A], sub: MyList[A]): Boolean = (sup, sub) match {
    case (MyNil, MyNil) => true
    case (_, MyNil) => true
    case (MyNil, _) => false
    case (Cons(x, sup), Cons(y, sub)) => if (x == y) hasSubsequence(sup, sub) else false
  }
}


sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // 3.25
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  // 3.26
  def maximun(tree: Tree[Int]): Int = tree match {
    case Leaf(x) => x
    case Branch(left, right) => maximun(left) max maximun(right)
  }

  // 3.27
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 0
    case Branch(left, right) => 1 + depth(left) max depth(right)
  }

  // 3.28
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(x) => Leaf(f(x))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  // 3.29
  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
    case Leaf(x) => f(x)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

  def mapViaFold[A, B](tree: Tree[A])(f: A => B): Tree[B] =
  // need to write down tpye
    fold(tree)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

  def sizeViaFold[A](tree: Tree[A]): Int =
    fold(tree)(_ => 1)(1 + _ + _)

  def maximunViaFold(tree: Tree[Int]): Int =
    fold(tree)(x => x)(_ max _)

  def depthViaFold[A](tree: Tree[A]): Int =
    fold(tree)(x => 0)((x, y) => 1 + (x max y))

  // Note the type annotation required on the expression Leaf(f(a)). Without this annotation, we get
  // an error like this:
  // type mismatch;
  // found : fpinscala.datastructures.Branch[B]
  // required: fpinscala.datastructures.Leaf[B]
  // fold(t)(a => Leaf(f(a)))(Branch(_,_))
  // ^
  // This error is an unfortunate consequence of Scala using subtyping to encode algebraic data types.
  // Without the annotation, the result type of the fold gets inferred as Leaf[B] and it is then expected
  // that the second argument to fold will return Leaf[B], which it doesn’t (it returns Branch[B]). Really,
  // we’d prefer Scala to infer Tree[B] as the result type in both cases. When working with algebraic data
  // types in Scala, it’s somewhat common to define helper functions that simply call the corresponding
  // data constructors but give the less specific result type:

  def leaf[A](a: A): Tree[A] = Leaf(a)

  def branch[A](l: Tree[A], r: Tree[A]): Tree[A] = Branch(l, r)
}
