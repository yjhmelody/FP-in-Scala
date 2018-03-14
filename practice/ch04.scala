case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

trait Option[+A] {
    
    // 4.1
    def map[B](f: A => B): Option[B] = this match {
        case None => None
        case Some(x) => Some(f(x))
    }

    def getOrElse[B >: A](default: => B): B = this match {
        case None => default
        case Some(x) => x
    }

    def flatMap[B](f: A => Option[B]): Option[B] = this match {
        case None => None
        case Some(x) => f(x)
    }

    def flatMap2[B](f: A => Option[B]): Option[B] = 
        map(f) getOrElse None


    def orElse[B >: A](ob: Option[B]): Option[B] = this match {
        case None => ob
        case _ => this
    }

    def orElse2[B >: A](ob: Option[B]): Option[B] = 
        this map (Some(_)) getOrElse ob

    def filter(f: A => Boolean): Option[A] = this match {
        case Some(x) if f(x) => this
        case _ => None
    }

    def filter2(f: A => Boolean): Option[A] =
        flatMap(a => if(f(a)) Some(a) else None)


}

object Option {
    def mean(xs: Seq[Double]): Option[Double] =
    if(xs.isEmpty) None
    else Some(xs.sum / xs.length)

    // 4.2
    def variance(xs: Seq[Double]): Option[Double] = 
        mean(xs) flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
        
    def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

    def Try[A](a: => A): Option[A] = 
        try Some(a)
        catch {case e: Exception => None}
    
    // 4.3
    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
        // aa is A which is in Option a, bb is B which is in  Option b.

        // This is the callback hell.
        a flatMap (aa => b map (bb => f(aa, bb)))
    
    def map2_2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
        for {
            aa <- a
            bb <- b
        } yield f(aa, bb)
    
    // 4.4
    def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
        case Nil => Some(Nil)
        // xx is a A and flatMap returns Option[List[A]]. The map(f: List[A] => List[A]): List[A].
        // So xx::_ means A::List[A].
        case x::xs => x flatMap (xx => sequence(xs) map (xx::_))
    }

    // It can also be implemented using foldRight and map2. The type annotation on foldRight is needed
    // here; otherwise Scala wrongly infers the result type of the fold as Some[Nil.type] and reports a
    // type error (try it!). This is an unfortunate consequence of Scala using subtyping to encode algebraic
    // data types.
    def sequence2[A](a: List[Option[A]]): Option[List[A]] =
        a.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2(x, y)(_ :: _))


    // 4.5
    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
        case Nil => Some(Nil)
        // map2 returns Option[_ :: _] which is Option[List[B]], because f(h) is Option[B].
        case h::t => map2(f(h), traverse(t)(f))(_ :: _)
    }

    // This is so similar to sequence2, it just has one more function f.
    def traverse2[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
        a.foldRight[Option[List[B]]](Some(Nil))((h, t) => map2(f(h), t)(_ :: _))
    
    // Let f returns the input.
    def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
        traverse(a)(x => x)


}


val xs1: Option[Int] = Some(1)
val xs2: Option[Int] = None
val xs3: Option[Double] = Some(2.0)

println(xs1.map(_ * 2))
println(xs2.map(_ * 2))

println(xs3.flatMap((x) => Some(x + 2)))
println(xs3.flatMap2((x) => Some(x + 2)))

println(xs1.getOrElse(233))
println(xs2.getOrElse(() => 233))

println(xs1.orElse(Some(233)))
println(xs2.orElse2(Some(233)))

println(xs1.filter(_ == 2))
println(xs2.filter2(_ == 2))
println(xs3.filter(_ == 2))

println(Option.lift((x:Int) => x * 2)(xs1))
println(Option.lift((x:Double) => x * 2)(xs3))

println(Option.mean(Seq[Double](1,2,3,4)))
println(Option.mean(Seq[Double]()))

println(Option.Try(2))
println(Option.Try(None))
println("4.1")

println(Option.variance(Seq[Double](1,2,3,4)))
println("4.2")

println(Option.map2(xs1, xs3)((x, y) => x + y))
println("4.3")

println(Option.sequence(List[Option[Int]](Some(1), Some(2), Some(3))))
println(Option.sequence(List[Option[Int]](Some(1), Some(2), None)))
println(Option.sequence2(List[Option[Int]](Some(1), Some(2), Some(3))))
println(Option.sequence2(List[Option[Int]](Some(1), Some(2), None)))
println("4.4")

println(Option.traverse(List[Option[Int]](Some(1), Some(2), Some(3)))(Option.lift(_ * 2)))
println(Option.traverse2(List[Option[Int]](Some(1), Some(2), None))(Option.lift(_ * 2)))
println("4.5")



case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

trait Either[+E, +A] {
    // 4.6
    def map[B](f: A => B): Either[E, B] = this match {
        case Left(e) => Left(e)
        case Right(a) => Right(f(a))
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
        case Left(e) => Left(e)
        case Right(a) => f(a)
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
        case Left(_) => b
        case Right(a) => Right(a)
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
        for {
            a <- this
            b1 <- b
        } yield f(a, b1)
}

object Either {
    def mean(xs: IndexedSeq[Double]): Either[String, Double] =
        if(xs.isEmpty)
            Left("mean of empty list")
        else 
            Right(xs.sum / xs.length)
    
    def safeDiv(x: Int, y: Int): Either[Exception, Int] =
        try Right(x / y)
        catch {case e: Exception => Left(e)}
    
    def Try[A](a: => A): Either[Exception, A] =
        try Right(a)
        catch{case e: Exception => Left(e)}
}


println(Left("error").map("error" + _))
println(Right(233).map(2 * _))

println(Left("error").flatMap(x => Left("error2")))
println(Right(233).flatMap(x => Right(x * 2)))
println(Right(233).flatMap(x => Left("error")))

println(Left("error").orElse(Right(233)))
println(Right(233).orElse(Left("error")))
println(Left(233).orElse(Left("error")))

println(Left(1).map2(Left(2))((x, y) => y))
println(Right(233).map2(Right(233))((x, y) => x + y))
println(Right(233).map2(Left("error"))((x, y) => y))
println(Left("error").map2(Right(233))((x, y) => y))

println("4.6")