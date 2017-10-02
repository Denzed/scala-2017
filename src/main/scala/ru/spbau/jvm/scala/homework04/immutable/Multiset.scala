package ru.spbau.jvm.scala.homework04.immutable

import scala.collection.GenTraversableOnce

class Multiset[+A](val data: List[(A, Int)]) {
  // determines whether given element is contained in the multiset
  def apply[B >: A](element: B): Boolean =
    data.exists(tuple => tuple._1 == element)

  // get the element
  def get[B >: A](element: B): Option[B] =
    data.find(tuple => tuple._1 == element)
        .map(_._1)

  // filters elements by predicate
  def filter(p: (_ <: A) ⇒ Boolean): Multiset[A] =
    new Multiset(data.filter(tuple => p(tuple._1)))

  // applies a given function to each element
  def map[B](f: (A) ⇒ B): Multiset[B] =
    new Multiset(data.map(tuple => (f(tuple._1), tuple._2)))

  // maps each element to a some Traversable container and joins
  def flatMap[B](f: (A) ⇒ GenTraversableOnce[B]): Multiset[B] =
    new Multiset(data
      .flatMap(tuple => f(tuple._1)
      .toSeq.map(x => (x, tuple._2))))

  // multiset intersection
  def &[B >: A](other: Multiset[B]): Multiset[B] = {
    val otherMap = Map(other.data :_*)
    new Multiset[B](for {
      (x, cnt) <- data if otherMap.keySet(x)
    } yield (x, Math.min(cnt, otherMap(x))))
  }

  private[this] def downcast[B >: A](): Multiset[B] =
    map(identity)

  // multiset union
  def |[B >: A](other: Multiset[B]): Multiset[B] = {
    val thisMap = Map(downcast[B]().data :_*)
    val otherMap = Map(other.data :_*)
    new Multiset[B]((for {
      x <- thisMap.keySet | otherMap.keySet
    } yield (x, thisMap.getOrElse(x, 0) + otherMap.getOrElse(x, 0)))
      .toList)
  }
}

object Multiset {
  def apply[A](elements: A*): Multiset[A] =
    new Multiset[A](elements
      .groupBy(identity)
      .mapValues(_.length)
      .toList)

  def unapplySeq[A](multiset: Multiset[A]): Option[Seq[A]] =
    Some(multiset.data.flatMap(tuple => Seq.fill(tuple._2)(tuple._1)))
}
