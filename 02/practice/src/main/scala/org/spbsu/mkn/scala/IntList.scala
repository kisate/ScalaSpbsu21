package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.IntList._

import scala.annotation.tailrec

sealed trait IntList {
  def head: Int
  def tail: IntList
  def drop(n: Int): IntList
  def take(n: Int): IntList
  def map(f: Int => Int): IntList
  def ::(elem: Int): IntList = IntListImpl(elem, this)
}

case class IntListImpl(head : Int, tail : IntList) extends IntList {
  override def drop(n: Int): IntList = tail match {
    case _ if n <= 0 => this
    case IntNil if n == 1 => IntNil
    case _ => tail.drop(n-1)
  }

  override def take(n: Int): IntList = tail match {
    case _ if n <= 0 => IntNil
    case IntNil if n == 1 => this
    case _ => IntListImpl(head, tail.take(n-1))
  }

  override def map(f: Int => Int): IntList = IntListImpl(f(head), tail.map(f))
}

object IntList {
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")
  def fromSeq(seq: Seq[Int]): IntList = seq match {
    case h :: t => IntListImpl(h, fromSeq(t))
    case Nil => IntNil
  }
  @tailrec
  def foldLeft[B](ini: B)(f: (B, Int) => B, intList: IntList): B = intList match {
    case IntNil => ini
    case IntListImpl(head, tail) => foldLeft(f(ini, head))(f, tail)
  }
  def sum(intList: IntList): Int      = intList match {
    case IntNil => undef
    case _ => foldLeft(0)(_ + _, intList)
  }
  def size(intList: IntList): Int     = foldLeft(0)({case (n, _) => n + 1}, intList)
}

object IntNil extends IntList {
  override def head: Int = undef
  override def tail: IntList = undef
  override def drop(n: Int): IntList = undef
  override def take(n: Int): IntList = undef
  override def map(f: Int => Int): IntList = IntNil
}