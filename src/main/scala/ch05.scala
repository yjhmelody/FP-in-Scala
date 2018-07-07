package com.yjhmelody.fp.ch5

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
}

sealed trait Stream[+A] {

  import Stream._

  def headOption(): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  // 5.1
  def toList(): List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList()
  }

  def toList2: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _ => acc
    }

    go(this, Nil).reverse
  }

  // 5.2
  def take(n: Int): Stream[A] = {
    if (n <= 0) Empty
    else this match {
      case Empty => Empty
      case Cons(h, t) => cons(h(), t().take(n - 1))
    }
  }

  @annotation.tailrec
  final def drop(n: Int): Stream[A] = {
    if (n <= 0) this
    else this match {
      case Empty => Empty
      case Cons(_, t) => t().drop(n - 1)
    }
  }

  // 5.3
  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if f(h()) => cons(h(), t().takeWhile(f))
    case _ => Empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists2(p: A => Boolean): Boolean =
    foldRight(false)((a, acc) => p(a) || acc)

  // 5.4
  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) && t().forAll(p)
    case _ => true
  }

  def forAll2(p: A => Boolean): Boolean =
    foldRight(true)((a, acc) => p(a) && acc)

  // 5.5
  def takeWhile2(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, acc) => if (f(a)) cons(a, acc) else empty[A])

  // 5.6
  def headOption2(): Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  // 5.7
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, acc) => cons(f(a), acc))
}