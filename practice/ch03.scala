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
        // case Nil => sys.error("tail of empty list")    
        case Nil => Nil
        case Cons(_, xs) => xs
    }

    // 3.3
    def setHead[A](xs: List[A], x: A): List[A] = xs match {
        // case Nil => sys.error("tail of empty list")        
        case Nil => Cons(x, Nil)
        case Cons(_, xs) => Cons(x, xs)
    }

    // 3.4
    def drop[A](xs: List[A], n: Int): List[A] = 
        if (n <= 0) xs
        else xs match {
            case Nil => Nil
            case Cons(_, t) => drop(t, n-1)
        }

    // 3.5
    def dropWhile[A](xs: List[A], f: A => Boolean): List[A] = xs match {
        case Cons(x, xs) if(f(x)) => dropWhile(xs, f)
        case _ => xs
    }

    // 3.6
    def init[A](xs: List[A]): List[A] = xs match {
        // case Nil => sys.error("init of empty list")
        case Nil => Nil
        case Cons(_, Nil) => Nil
        case Cons(x, xs) => Cons(x, init(xs))
    }

    def init2[A](xs: List[A]): List[A] = {
        import collection.mutable.ListBuffer
        val buf = new ListBuffer[A]
        @annotation.tailrec
        def go(cur: List[A]): List[A] = cur match {
        // case Nil => sys.error("init of empty list")
        case Nil => Nil
        case Cons(_,Nil) => List(buf.toList: _*)
        case Cons(h,t) => buf += h; go(t)
        }
        go(xs)
    }

    // example 3.2
    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    def sum2(xs: List[Int]) = 
        foldRight(xs, 0)(_ + _)
    
    def product2(xs: List[Double]) = 
        foldRight(xs, 1.0)(_ * _)

    // 3.9
    def length[A](xs: List[A]): Int = 
        foldRight(xs, 0)((_, acc) => acc + 1)
    
    // 3.10
    @annotation.tailrec
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
        case Nil => z
        case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

    // 3.11
    def sum3(xs: List[Int]) = foldLeft(xs, 0)(_ + _)
    
    def product3(xs: List[Double]) = foldLeft(xs, 1.0)(_ * _)

    def length2[A](xs: List[A]): Int = foldLeft(xs, 0)((acc, _) => acc + 1)

    // 3.12
    def reverse[A](xs: List[A]): List[A] = 
       foldLeft(xs, List[A]())((acc, h) => Cons(h, acc))

    // 3.13
    // 

    // 3.14
    def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
        foldRight(l, r)(Cons(_, _))
}

val ex1: List[Double] = Nil
val ex2: List[Int] = Cons(1, Nil)
val ex3: List[String] = Cons("a", Cons("b", Nil))
var ex4 = List(1,2,3,4,5)

val x = ex4 match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => 3
    case Cons(h, t) => h + List.sum(t)
    case _ => 101
}
println(x)
println("3.1")

println(List.tail(ex1))
println(List.tail(ex2))
println(List.tail(ex3))
println(List.tail(ex4))
println("3.2")

println(List.setHead(ex1, 1.0))
println(List.setHead(ex2, 2))
println(List.setHead(ex3, "c"))
println(List.setHead(ex4, 0))
println("3.3")

println(List.drop(ex1, 1))
println(List.drop(ex2, 2))
println(List.drop(ex3, 3))
println(List.drop(ex4, 4))
println("3.4")

println(List.dropWhile(ex1, (x: Double) => x == 0))
println(List.dropWhile(ex2, (x: Int) => x == 1))
println(List.dropWhile(ex3, (x: String) => x == "b"))
println(List.dropWhile(ex4, (x: Int) => x == 3))
println("3.5")

println(List.init(ex1))
println(List.init(ex2))
println(List.init(ex3))
println(List.init(ex4))
println("3.6")

println(List.init2(ex1))
println(List.init2(ex2))
println(List.init2(ex3))
println(List.init2(ex4))
println("3.6-2")

println(List.foldRight(List(1,2,3,4), Nil:List[Int])(Cons(_, _)))
println("3.8")
// foldRight(Cons(1, Cons(2, Cons(3, Nil))), Nil:List[Int])(Cons(_,_))
// Cons(1, foldRight(Cons(2, Cons(3, Nil)), Nil:List[Int])(Cons(_,_)))
// Cons(1, Cons(2, foldRight(Cons(3, Nil), Nil:List[Int])(Cons(_,_))))
// Cons(1, Cons(2, Cons(3, foldRight(Nil, Nil:List[Int])(Cons(_,_)))))
// Cons(1, Cons(2, Cons(3, Nil)))

println(List.length(ex1))
println(List.length(ex2))
println(List.length(ex3))
println(List.length(ex4))
println("3.9")

println(List.foldLeft(List(1,2,3,4), 0)(_ + _))
println("3.10")

println(List.sum3(ex4))
println(List.product3(ex1))
println(List.product3(List[Double](1,2,3,4)))
println("3.11")

println(List.reverse(ex4))
println("3.12")


println(List.append(ex4, List(6, 7)))
println("3.14")