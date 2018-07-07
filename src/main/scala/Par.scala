package com.yjhmelody.fp.ch7

import java.util.concurrent.TimeUnit

object Par {
  def sortPar(pa: Par[List[Int]]): Par[List[Int]] = map(pa)(_.sorted)

  type Par[A] = ExecutorService => Future[A]

  class ExecutorService {
    def submit[A](a: Callable[A]): Future[A] = {
      UnitFuture(a.call)
    }
  }

  trait Callable[A] {
    /**
      * 惰性类型A
      *
      * @return
      */
    def call: A
  }

  trait Future[A] {
    def get: A

    def get(timeout: Long, unit: TimeUnit): A

    def cancel(evenIfRunning: Boolean): Boolean

    def isDone: Boolean

    def isCancelled: Boolean
  }

  case class Map2Future[A, B, C](a: Future[A], b: Future[B],
                                 f: (A, B) => C) extends Future[C] {
    @volatile var cache: Option[C] = None

    def isDone = cache.isDefined

    def isCancelled = a.isCancelled || b.isCancelled

    def cancel(evenIfRunning: Boolean) =
      a.cancel(evenIfRunning) || b.cancel(evenIfRunning)

    def get = compute(Long.MaxValue)

    def get(timeout: Long, units: TimeUnit): C =
      compute(TimeUnit.MILLISECONDS.convert(timeout, units))

    private def compute(timeoutMs: Long): C = cache match {
      case Some(c) => c
      case None =>
        val start = System.currentTimeMillis
        val ar = a.get(timeoutMs, TimeUnit.MILLISECONDS)
        val stop = System.currentTimeMillis;
        val at = stop - start
        val br = b.get(timeoutMs - at, TimeUnit.MILLISECONDS)
        val ret = f(ar, br)
        cache = Some(ret)
        ret
    }
  }

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def isDone: Boolean = true

    override def get(timeout: Long, units: TimeUnit) = get

    override def isCancelled: Boolean = false

    override def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def unit[A](a: A): Par[A] = {
    (es: ExecutorService) => UnitFuture(a)
  }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  /**
    * 将一个函数转为异步
    *
    * @param f
    * @tparam A
    * @tparam B
    * @return
    */
  def asyncF[A, B](f: A => B): A => Par[B] = {
    (a: A) => lazyUnit(f(a))
  }

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def sequenceSimple[A](ps: List[Par[A]]): Par[List[A]] = {
    ps.foldLeft[Par[List[A]]](unit(List()))((acc, x) => map2(x, acc)(_ :: _))
    //    ps.foldRight[Par[List[A]]](unit(List()))((h, t) => map2(h, t)(_ :: _))
  }

  def sequence[A](as: List[Par[A]]): Par[List[A]] =
    map(sequenceBalanced(as.toIndexedSeq))(_.toList)

  def sequenceRight[A](as: List[Par[A]]): Par[List[A]] =
    as match {
      case List() => unit(List())
      case h :: t => map2(h, fork(sequenceRight(t)))(_ :: _)
    }

  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (as.isEmpty) unit(Vector())
    else if (as.length == 1) map(as.head)(a => Vector(a))
    else {
      val (l, r) = as.splitAt(as.length / 2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] =
      as map (asyncF((a: A) => if (f(a)) List(a) else List()))
    // flatten is a convenience method on `List` for concatenating a list of lists
    map(sequence(pars))(_.flatten)
  }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      override def call: A = a(es).get
    })

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)
}
