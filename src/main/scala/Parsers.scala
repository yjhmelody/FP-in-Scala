package com.yjhmelody.fp.ch9

object Parsers {
  def main(args: Array[String]): Unit = {

  }
}



trait Parsers[ParseError, Parser[+_]] {
  self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def char(c: Char): Parser[Char] = map(string(c.toString))(_.charAt(0))
  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]
  def map[A, B](a: Parser[A])(f: A => B): Parser[B]
  def map2[A, B, C](a: Parser[A], b: => Parser[B])(f: (A, B) => C): Parser[C] = map(product(a, b))(f.tupled)
  def many[A](a: Parser[A]): Parser[List[A]] = or(map2(a, many(a))(_ :: _), succeed(List()))
  def many1[A](a: Parser[A]): Parser[List[A]] = map2(a, many(a))(_ :: _)
  def product[A, B](a: Parser[A], b: => Parser[B]): Parser[(A, B)]
  def unbiasL[A, B, C](p: ((A, B), C)): (A, B, C) = (p._1._1, p._1._2, p._2)
  def unbiasR[A, B, C](p: (A, (B, C))): (A, B, C) = (p._1, p._2._1, p._2._2)
  def succeed[A](a: A): Parser[A] = map(string(""))(_ => a)
  def slice[A](a: Parser[A]): Parser[String]
  def listOfN[A](n: Int, a: Parser[A]): Parser[List[A]] =
    if (n < 0) succeed(List())
    else map2(a, listOfN(n-1, a))(_ :: _)

  def wrap[A](p: => Parser[A]): Parser[A]

  implicit def string(s: String): Parser[String]
  implicit def operations[A](a: Parser[A]) = ParserOps[A](a)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def **[A, B](a: Parser[A], b: Parser[B]): Parser[(A, B)] = self.product(a, b)
    def product[A, B](a: => Parser[A], b: => Parser[B]): Parser[(A, B)] = self.product(a, b)
  }

}
