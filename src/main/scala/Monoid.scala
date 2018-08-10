package com.yjhmelody.fp.ch10


trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero(): A
}

