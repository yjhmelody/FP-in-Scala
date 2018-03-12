case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

trait Option[+A] {
    
    // 4.1
    def map[B](f: A => B): Option[B] = this match {
        case None => None
        case Some(x) => Some(f(x))
    }

    def flatMap[B](f: A => Option[B]): Option[B] = this match {
        case None => None
        case Some(x) => f(x)
    }

    def getOrElse[B >: A](default: => B): B = this match {
        case None => default
        case Some(x) => x
    }

    def orElse[B >: A](ob: Option[B]): Option[B] = this match {
        case None => ob
        case _ => this
    }

    def filter(f: A => Boolean): Option[A] = this match {
        case Some(x) if(f(x)) => Some(x)
        case _ => None
    }
}

val xs1: Option[Int] = Some(1)
val xs2: Option[Int] = None
val xs3: Option[Double] = Some(2.0)

println(xs1.map(_ * 2))
println(xs2.map(_ * 2))

println(xs3.flatMap((x) => Some(x + 2)))
println(xs1.getOrElse(233))
println(xs2.getOrElse(() => 233))

println(xs1.orElse(Some(233)))
println(xs2.orElse(Some(233)))

println(xs1.filter(_ == 2))
println(xs2.filter(_ == 2))
println(xs3.filter(_ == 2))
println("4.1")
