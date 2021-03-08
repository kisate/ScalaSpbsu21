package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.MyGenericList._

import scala.annotation.tailrec

sealed trait MyGenericList[+A] {
  def head: A
  def tail: MyGenericList[A]
  def drop(n: Int): MyGenericList[A]
  def take(n: Int): MyGenericList[A]
  def map[B](f: A => B): MyGenericList[B]
  def ::[B >: A](elem: B): MyGenericList[B] = GenericListImpl(elem, this)
}

case class GenericListImpl[A](head : A, tail : MyGenericList[A]) extends MyGenericList[A] {
  override def drop(n: Int): MyGenericList[A] = tail match {
    case _ if n < 0 => undef
    case _ if n == 0 => this
    case MyNil if n == 1 => MyNil
    case _ => tail.drop(n-1)
  }

  override def take(n: Int): MyGenericList[A] = tail match {
    case _ if n < 0 => undef
    case _ if n == 0 => MyNil
    case MyNil if n == 1 => this
    case _ => GenericListImpl(head, tail.take(n-1))
  }

  override def map[B](f: A => B): MyGenericList[B] = GenericListImpl(f(head), tail.map(f))
}

object MyGenericList {
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")
  def fromSeq[A](seq: Seq[A]): MyGenericList[A] = seq match {
    case h :: t => GenericListImpl(h, fromSeq(t))
    case Nil => MyNil
  }
  @tailrec
  def foldLeft[A, B](ini: B)(genericList: MyGenericList[A])(f: (B, A) => B): B = genericList match {
    case MyNil => ini
    case GenericListImpl(head, tail) => foldLeft(f(ini, head))(tail)(f)
  }
  def size[A](genericList: MyGenericList[A]): Int     = foldLeft(0)(genericList)({case (n, _) => n + 1})
  def sum(genericList: MyGenericList[Int]): Int      = genericList match {
    case MyNil => undef
    case _ => foldLeft(0)(genericList)(_ + _)
  }
}

object MyNil extends MyGenericList[Nothing] {
  override def head: Nothing = undef
  override def tail: MyGenericList[Nothing] = undef
  override def drop(n: Int): MyGenericList[Nothing] = undef
  override def take(n: Int): MyGenericList[Nothing] = undef
  override def map[B](f: Nothing => B): MyGenericList[B] = MyNil
}