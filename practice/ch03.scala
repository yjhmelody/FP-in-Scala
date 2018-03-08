sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
    def sum(ints: List[Int]): Int = ints match {
        case Nil => 0
        case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
        case Nil => 1.0
        case Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] = 
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))

    def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
        case Nil => a2
        case Cons(h, t) => Cons(h, append(t, a2))
    }

    // 3.2
    def tail[A](xs: List[A]): List[A] = xs match {
        case Nil => Nil
        case Cons(x, xs) => xs
    }

    // 3.3
    def setHead[A](x: A, xs: List[A]): List[A] = xs match {
        case Nil => Cons(x, Nil)
        case Cons(_, xs) => Cons(x, xs)
    }

    // 3.4
    def drop[A](xs: List[A], n: Int): List[A] = n match {
        case 0 => xs
        case m => drop(tail(xs), m-1)
    }

    // 3.5
    def dropWhile[A](xs: List[A], f: A => Boolean): List[A] = xs match {
        case Nil => Nil
        case Cons(x, xss) => if (f(x)) dropWhile(xss, f) else Cons(x, dropWhile(xss, f))
    }

    // 3.6
    def init[A](xs: List[A]): List[A] = xs match {
        case Nil => Nil
        case Cons(x, Nil) => Nil
        case Cons(x, xs) => Cons(x, init(xs))
    }
}

val ex1: List[Double] = Nil
val ex2: List[Int] = Cons(1, Nil)
val ex3: List[String] = Cons("a", Cons("b", Nil))
var ex4 = List(1,2,3,4,5)

// 3.1
val x = ex4 match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => 3
    case Cons(h, t) => h + List.sum(t)
    case _ => 101
}
println(x)


// 3.2 
println(List.tail(ex1))
println(List.tail(ex2))
println(List.tail(ex3))
println(List.tail(ex4))
println("3.2")


// 3.3
println(List.setHead(1.0, ex1))
println(List.setHead(2, ex2))
println(List.setHead("c", ex3))
println(List.setHead(0, ex4))
println("3.3")


// 3.4
println(List.drop(ex1, 1))
println(List.drop(ex2, 2))
println(List.drop(ex3, 3))
println(List.drop(ex4, 4))
println("3.4")

// 3.5
println(List.dropWhile(ex1, (x: Double) => x == 0))
println(List.dropWhile(ex2, (x: Int) => x == 1))
println(List.dropWhile(ex3, (x: String) => x == "b"))
println(List.dropWhile(ex4, (x: Int) => x == 3))
println("3.5")


// 3.6
println(List.init(ex1))
println(List.init(ex2))
println(List.init(ex3))
println(List.init(ex4))
println("3.6")
