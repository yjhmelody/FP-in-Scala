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
        // aa is A which is in a, bb is B which is in b.
        a flatMap (aa => b map (bb => f(aa, bb)))
    
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