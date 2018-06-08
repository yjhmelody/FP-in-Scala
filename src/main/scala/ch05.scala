// sealed trait Stream[+A]
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def main(args: Array[String]): Unit = {
    val s1 = Stream(1, 2, 3, 4)
    println(s1.toList)
    println(s1.take(2).toList)
    println(s1.drop(2).toList)
    println(s1.takeWhile(_ < 4).toList2)
  }

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

trait Stream[+A] {
    // 5.1
    def toList: List[A] = this match {
        case Empty => Nil
        case Cons(h, t) => h()::t().toList
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
        if(n <= 0) Empty
        else this match {
            case Empty => Empty
            case Cons(h, t) => Stream.cons(h(), t().take(n-1))
        }
    }

    @annotation.tailrec
    final def drop(n: Int): Stream[A] = {
        if(n <= 0) this
        else this match {
            case Empty => Empty
            case Cons(_, t) => t().drop(n-1)
        }
    }

    // 5.3
    def takeWhile(f: A => Boolean): Stream[A] = this match {
        case Cons(h, t) if f(h()) => Stream.cons(h(), t().takeWhile(f))
        case _ => Empty 
    }
}


