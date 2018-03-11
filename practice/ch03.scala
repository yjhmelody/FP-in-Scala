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
    def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B =
        foldLeft(reverse(l), z)((b,a) => f(a,b))

    def foldRightViaFoldLeft_1[A,B](l: List[A], z: B)(f: (A,B) => B): B =
        foldLeft(l, (b:B) => b)((g,a) => b => g(f(a,b)))(z)

    def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B =
        foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)

    // 3.14
    def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
        foldRight(l, r)(Cons(_, _))

    // 3.15
    def concat[A](xs: List[List[A]]): List[A] = 
        foldRight(xs, Nil:List[A])(append)

    // 3.16
    def plusOne(xs: List[Int]): List[Int] = xs match {
        case Nil => Nil
        case Cons(x, xs) => Cons(x+1, plusOne(xs))
    }

    // 3.16-2
    def add1(xs: List[Int]): List[Int] = 
        foldRight(xs, Nil:List[Int])((h, t) => Cons(h+1, t))

    // 3.17
    def doubleToString(xs: List[Double]): List[String] = xs match {
        case Nil => Nil
        case Cons(x, xs) => Cons(x.toString(), doubleToString(xs))
    }

    // 3.17-2
    def doubleToString2(xs: List[Double]): List[String] =
        foldRight(xs, Nil:List[String])((h, t) => Cons(h.toString, t))

    // 3.18*

//     A natural solution is using foldRight, but our implementation of foldRight is not stack-safe. We
// can use foldRightViaFoldLeft to avoid the stack overflow (variation 1), but more commonly, with
// our current implementation of List, map will just be implemented using local mutation (variation
// 2). Again, note that the mutation isn’t observable outside the function, since we’re only mutating a
// buffer that we’ve allocated.

    def map[A, B](xs: List[A])(f: A => B): List[B] = xs match {
        case Nil => Nil
        case Cons(x, xs) => Cons(f(x), map(xs)(f))
    }

    // 3.19
    def filter[A](xs: List[A])(f: A => Boolean): List[A] = {
        def loop(xs: List[A], prev: A): List[A] = {
            xs match {
                case Nil => Nil
                case Cons(x, xs) if(f(x)) => Cons(x, loop(xs, x))
                case Cons(x, xs) => loop(xs, x)
            }
        }

        xs match {
            case Nil => Nil
            case Cons(x, xs) => loop(Cons(x, xs), x)
        }
    }
    
    def filter2[A](xs: List[A])(f: A => Boolean): List[A] =
        foldRight(xs, Nil:List[A])((h, t) => if(f(h)) Cons(h, t) else t)

    def filter3[A](xs: List[A])(f: A => Boolean): List[A] = {
         val buf = new collection.mutable.ListBuffer[A]
         def loop(xs: List[A]): Unit = xs match {
             case Nil => ()
             case Cons(h, t) => if (f(h)) buf += h; loop(t)
         }

         loop(xs)
         List(buf.toList: _*)
         // converting from the standard Scala list to the list we've defined here
    }
    // 3.20
    // This could also be implemented directly using `foldRight`.
    // mine
    def flatMap[A, B](xs: List[A])(f: A => List[B]): List[B] = xs match {
        case Nil => Nil
        case Cons(x, xs) => append(f(x), flatMap(xs)(f))
    }

    def flatMap2[A, B](xs: List[A])(f: A => List[B]): List[B] = 
        concat(map(xs)(f))
    
    // hard
    // def flatMap3[A, B](xs: List[A])(f: A => List[B]): List[B] =
        // foldRight(xs, Nil:List[B])((x, xs) => Cons(f(x), xs))

    // 3.21
    // Regrad inner List as a container. If true return List(x) else Nil, and Nil will be droped by flatMap.
    def filterViaFlatMap[A](xs: List[A])(f: A => Boolean): List[A] = 
        flatMap(xs)(x => if(f(x)) List(x) else Nil)

    // 3.22
//     To match on multiple values, we can put the values into a pair and match on the pair, as shown next,
// and the same syntax extends to matching on N values (see sidebar “Pairs and tuples in Scala” for more
// about pair and tuple objects). You can also (somewhat less conveniently, but a bit more efficiently)
// nest pattern matches: on the right hand side of the =>, simply begin another match expression. The
// inner match will have access to all the variables introduced in the outer match.
    // mine
    def plus[A](xs: List[A], ys: List[A])(f: (A, A) => A): List[A] = (xs, ys) match {
        case (Nil, _) => Nil
        case (_, Nil) => Nil
        case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), plus(xs, ys)(f))
    }

    def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a,b) match {
        case (Nil, _) => Nil
        case (_, Nil) => Nil
        case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, addPairwise(t1,t2))
    }

    // 3.23
    def zipWith[A, B, C](xs: List[A], ys: List[B])(f: (A, B) => C): List[C] = (xs, ys) match {
        case (Nil, _) => Nil
        case (_, Nil) => Nil        
        case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
    }
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

println(List.concat(List[List[Int]](List(1,2,3), List(4,5,6))))
println("3.15")

println(List.plusOne(ex4))
println(List.add1(ex4))
println("3.16")

println(List.doubleToString(List[Double](1.1,2.2,3.3,4.4)))
println(List.doubleToString2(List[Double](2.2, 10.00)))
println("3.17")

println(List.map(ex4)(_ * 2))
println("3.18")

println(List.filter(ex4)(x => x != 4))
println("3.19")

println(List.flatMap(List(1,2,3))(i => List(i, i)))
println("3.20")

println(List.plus(ex4, ex4)(_ + _))
println("3.22")

println(List.zipWith(ex4, ex4)(_ * _))