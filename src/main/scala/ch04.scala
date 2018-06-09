package com.yjhmelody.fp.ch4


case class MySome[+A](get: A) extends MyOption[A]

case object MyNone extends MyOption[Nothing]

trait MyOption[+A] {
  def flatMap2[B](f: A => MyOption[B]): MyOption[B] =
    map(f) getOrElse MyNone

  // 4.1
  def map[B](f: A => B): MyOption[B] = this match {
    case MyNone => MyNone
    case MySome(x) => MySome(f(x))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case MyNone => default
    case MySome(x) => x
  }

  def orElse[B >: A](ob: MyOption[B]): MyOption[B] = this match {
    case MyNone => ob
    case _ => this
  }

  def orElse2[B >: A](ob: MyOption[B]): MyOption[B] =
    this map (MySome(_)) getOrElse ob

  def filter(f: A => Boolean): MyOption[A] = this match {
    case MySome(x) if f(x) => this
    case _ => MyNone
  }

  def filter2(f: A => Boolean): MyOption[A] =
    flatMap(a => if (f(a)) MySome(a) else MyNone)

  def flatMap[B](f: A => MyOption[B]): MyOption[B] = this match {
    case MyNone => MyNone
    case MySome(x) => f(x)
  }

}

object MyOption {
  def main(args: Array[String]): Unit = {
    val xs1: MyOption[Int] = MySome(1)
    val xs2: MyOption[Int] = MyNone
    val xs3: MyOption[Double] = MySome(2.0)

    println(xs1.map(_ * 2))
    println(xs2.map(_ * 2))

    println(xs3.flatMap((x) => MySome(x + 2)))
    println(xs3.flatMap2((x) => MySome(x + 2)))

    println(xs1.getOrElse(233))
    println(xs2.getOrElse(() => 233))

    println(xs1.orElse(MySome(233)))
    println(xs2.orElse2(MySome(233)))

    println(xs1.filter(_ == 2))
    println(xs2.filter2(_ == 2))
    println(xs3.filter(_ == 2))

    println(MyOption.lift((x: Int) => x * 2)(xs1))
    println(MyOption.lift((x: Double) => x * 2)(xs3))

    println(MyOption.mean(Seq[Double](1, 2, 3, 4)))
    println(MyOption.mean(Seq[Double]()))

    println(MyOption.Try(2))
    println(MyOption.Try(MyNone))
    println("4.1")

    println(MyOption.variance(Seq[Double](1, 2, 3, 4)))
    println("4.2")

    println(MyOption.map2(xs1, xs3)((x, y) => x + y))
    println("4.3")

    println(MyOption.sequence(List[MyOption[Int]](MySome(1), MySome(2), MySome(3))))
    println(MyOption.sequence(List[MyOption[Int]](MySome(1), MySome(2), MyNone)))
    println(MyOption.sequence2(List[MyOption[Int]](MySome(1), MySome(2), MySome(3))))
    println(MyOption.sequence2(List[MyOption[Int]](MySome(1), MySome(2), MyNone)))
    println("4.4")

    println(MyOption.traverse(List[MyOption[Int]](MySome(1), MySome(2), MySome(3)))(MyOption.lift(_ * 2)))
    println(MyOption.traverse2(List[MyOption[Int]](MySome(1), MySome(2), MyNone))(MyOption.lift(_ * 2)))
    println("4.5")
  }

  def mean(xs: Seq[Double]): MyOption[Double] =
    if (xs.isEmpty) MyNone
    else MySome(xs.sum / xs.length)

  // 4.2
  def variance(xs: Seq[Double]): MyOption[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  def lift[A, B](f: A => B): MyOption[A] => MyOption[B] = _ map f

  def Try[A](a: => A): MyOption[A] =
    try MySome(a)
    catch {
      case e: Exception => MyNone
    }

  // 4.3
  def map2[A, B, C](a: MyOption[A], b: MyOption[B])(f: (A, B) => C): MyOption[C] =
  // aa is A which is in Option a, bb is B which is in  Option b.

  // This is the callback hell.
    a flatMap (aa => b map (bb => f(aa, bb)))

  def map2_2[A, B, C](a: MyOption[A], b: MyOption[B])(f: (A, B) => C): MyOption[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

  // 4.4
  def sequence[A](a: List[MyOption[A]]): MyOption[List[A]] = a match {
    case Nil => MySome(Nil)
    // xx is a A and flatMap returns Option[List[A]]. The map(f: List[A] => List[A]): List[A].
    // So xx::_ means A::List[A].
    case x :: xs => x flatMap (xx => sequence(xs) map (xx :: _))
  }

  // It can also be implemented using foldRight and map2. The type annotation on foldRight is needed
  // here; otherwise Scala wrongly infers the result type of the fold as Some[Nil.type] and reports a
  // type error (try it!). This is an unfortunate consequence of Scala using subtyping to encode algebraic
  // data types.
  def sequence2[A](a: List[MyOption[A]]): MyOption[List[A]] =
    a.foldRight[MyOption[List[A]]](MySome(Nil))((x, y) => map2(x, y)(_ :: _))


  // 4.5
  def traverse[A, B](a: List[A])(f: A => MyOption[B]): MyOption[List[B]] = a match {
    case Nil => MySome(Nil)
    // map2 returns Option[_ :: _] which is Option[List[B]], because f(h) is Option[B].
    case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
  }

  // This is so similar to sequence2, it just has one more function f.
  def traverse2[A, B](a: List[A])(f: A => MyOption[B]): MyOption[List[B]] =
    a.foldRight[MyOption[List[B]]](MySome(Nil))((h, t) => map2(f(h), t)(_ :: _))

  // Let f returns the input.
  def sequenceViaTraverse[A](a: List[MyOption[A]]): MyOption[List[A]] =
    traverse(a)(x => x)


}


case class MyLeft[+E](value: E) extends MyEither[E, Nothing]

case class MyRight[+A](value: A) extends MyEither[Nothing, A]

trait MyEither[+E, +A] {
  // 4.6
  def map[B](f: A => B): MyEither[E, B] = this match {
    case MyLeft(e) => MyLeft(e)
    case MyRight(a) => MyRight(f(a))
  }

  def flatMap[EE >: E, B](f: A => MyEither[EE, B]): MyEither[EE, B] = this match {
    case MyLeft(e) => MyLeft(e)
    case MyRight(a) => f(a)
  }

  def orElse[EE >: E, B >: A](b: => MyEither[EE, B]): MyEither[EE, B] = this match {
    case MyLeft(_) => b
    case MyRight(a) => MyRight(a)
  }

  def map2[EE >: E, B, C](b: MyEither[EE, B])(f: (A, B) => C): MyEither[EE, C] =
    for {
      a <- this
      b1 <- b
    } yield f(a, b1)
}

object MyEither {

  def main(args: Array[String]): Unit = {

    println(MyLeft("error").map("error" + _))
    println(MyRight(233).map(2 * _))

    println(MyLeft("error").flatMap(x => MyLeft("error2")))
    println(MyRight(233).flatMap(x => MyRight(x * 2)))
    println(MyRight(233).flatMap(x => MyLeft("error")))

    println(MyLeft("error").orElse(MyRight(233)))
    println(MyRight(233).orElse(MyLeft("error")))
    println(MyLeft(233).orElse(MyLeft("error")))

    println(MyLeft(1).map2(MyLeft(2))((x, y) => y))
    println(MyRight(233).map2(MyRight(233))((x, y) => x + y))
    println(MyRight(233).map2(MyLeft("error"))((x, y) => y))
    println(MyLeft("error").map2(MyRight(233))((x, y) => y))

    println("4.6")

    println(MyEither.sequence(List[MyEither[String, Int]](MyRight(1), MyRight(2), MyLeft("error"))))
    println(MyEither.sequence(List[MyEither[String, Int]](MyRight(1), MyRight(2))))
    println(MyEither.traverse(List[Int](1, 2, 3))(a => MyRight(a * 2)))
    println(MyEither.traverse(List[Int](1, 2, 3))(a => if (a != 3) MyRight(a * 2) else MyLeft("error")))
    println("4.7")

    // 4.8
    // There are a number of variations on Option and Either. If we want to accumulate multiple errors,
    // a simple approach is a new data type that lets us keep a list of errors in the data constructor that
    // represents failures:

    // trait Partial[+A,+B]
    // case class Errors[+A](get: Seq[A]) extends Partial[A,Nothing]
    // case class Success[+B](get: B) extends Partial[Nothing,B]

    // There is a type very similar to this called Validation in the Scalaz library. You can implement map,
    // map2, sequence, and so on for this type in such a way that errors are accumulated when possible
    // (flatMap is unable to accumulate errors–can you see why?). This idea can even be generalized
    // further–we don’t need to accumulate failing values into a list; we can accumulate values using
    // any user-supplied binary function.

    // It’s also possible to use Either[List[E],_] directly to accumulate errors, using different implementations
    // of helper functions like map2 and sequence.
  }

  def mean(xs: IndexedSeq[Double]): MyEither[String, Double] =
    if (xs.isEmpty)
      MyLeft("mean of empty list")
    else
      MyRight(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): MyEither[Exception, Int] =
    try MyRight(x / y)
    catch {
      case e: Exception => MyLeft(e)
    }

  def Try[A](a: => A): MyEither[Exception, A] =
    try MyRight(a)
    catch {
      case e: Exception => MyLeft(e)
    }

  // 4.7
  def traverse[E, A, B](es: List[A])(f: A => MyEither[E, B]): MyEither[E, List[B]] = es match {
    case Nil => MyRight(Nil)
    case h :: t => (f(h) map2 traverse(t)(f)) (_ :: _)
  }

  def traverse2[E, A, B](es: List[A])(f: A => MyEither[E, B]): MyEither[E, List[B]] =
    es.foldRight[MyEither[E, List[B]]](MyRight(Nil))((a, b) => f(a).map2(b)(_ :: _))

  def sequence[E, A](es: List[MyEither[E, A]]): MyEither[E, List[A]] =
    traverse(es)(x => x)
}