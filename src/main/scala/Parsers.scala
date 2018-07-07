package com.yjhmelody.fp.ch9

import scala.util.matching.Regex


trait Parsers[ParseError, Parser[+_]] {
  self => // so inner classes may call methods of trait

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def char(c: Char): Parser[Char] = map(string(c.toString))(_.charAt(0))

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]

  def map[A, B](p: Parser[A])(f: A => B): Parser[B] = flatMap(p)(f andThen succeed)

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  // 9.1
  def map2[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = map(product(p1, p2))(f.tupled)

  // 9.3
  def many[A](p: Parser[A]): Parser[List[A]] = or(map2(p, wrap(many(p)))(_ :: _), succeed(List()))

  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)

  // 9.7
  def product[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    flatMap(p1)(a => map(p2)(b => (a, b)))

  def unbiasL[A, B, C](p: ((A, B), C)): (A, B, C) = (p._1._1, p._1._2, p._2)

  def unbiasR[A, B, C](p: (A, (B, C))): (A, B, C) = (p._1, p._2._1, p._2._2)

  /*
   * A default `succeed` implementation in terms of `string` and `map`.
   * We leave `succeed` abstract, since `map` is defined below in terms of
   * `flatMap` and `succeed`, which would be a circular definition! But we include
   * the definition here in case implementations wish to use it
   * (say if they provide a custom implementation of `map`, breaking the cycle)
   */
  def defaultSucceed[A](a: A): Parser[A] = map(string(""))(_ => a)

  def succeed[A](a: A): Parser[A]

  def slice[A](p: Parser[A]): Parser[String]

  // 9.4
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n < 0) succeed(List())
    else map2(p, listOfN(n - 1, p))(_ :: _)

  // 9.5
  def wrap[A](p: => Parser[A]): Parser[A]

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def attempt[A](p: Parser[A]): Parser[A]

  implicit def regex(r: Regex): Parser[String]

  implicit def string(s: String): Parser[String]

  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def **[A, B](p1: Parser[A], p2: Parser[B]): Parser[(A, B)] = self.product(p1, p2)

    def product[A, B](p1: => Parser[A], p2: => Parser[B]): Parser[(A, B)] = self.product(p1, p2)
  }

}

object Parsers {

}
