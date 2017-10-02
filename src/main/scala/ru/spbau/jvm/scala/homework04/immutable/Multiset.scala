package ru.spbau.jvm.scala.homework04.immutable

import scala.collection.GenTraversableOnce

class Multiset[+A] private (val data: Map[A, Int]) {
  // determines whether given element is contained in the multiset
  def apply[B >: A](element: B): Boolean =
    data.exists(tuple => tuple._1 == element)

  // get the element
  def get[B >: A](element: B): Option[B] =
    data.find(tuple => tuple._1 == element)
        .map(_._1)

  // filters elements by predicate
  def filter(p: (A) ⇒ Boolean): Multiset[A] =
    new Multiset(data.filterKeys(p))

  // applies a given function to each element
  def map[B](f: (A) ⇒ B): Multiset[B] =
    new Multiset(data
      .groupBy(tuple => f(tuple._1))
      .mapValues(_.foldLeft(0)((a, tuple) => a + tuple._2)))

  // maps each element to a some Traversable container and joins
  def flatMap[B](f: (A) ⇒ GenTraversableOnce[B]): Multiset[B] = {
    def helper(tuple: (A, Int)): Multiset[B] =
      new Multiset[B](f(tuple._1)
        .seq.toSeq
        .groupBy(identity)
        .mapValues(_.length * tuple._2))
    data.foldLeft(Multiset[B]())((a, tuple) => a | helper(tuple))
  }

  // multiset intersection
  def &[B >: A](other: Multiset[B]): Multiset[B] = {
    new Multiset[B](for {
      (x, cnt) <- data if other.data.keySet(x)
    } yield (x, Math.min(cnt, other.data(x))))
  }

  private def downcast[B >: A](): Multiset[B] =
    map(identity)

  // multiset union
  def |[B >: A](other: Multiset[B]): Multiset[B] = {
    val downcastThis: Multiset[B] = downcast()
    new Multiset[B]((for {
      x <- downcastThis.data.keySet | other.data.keySet
    } yield (x, downcastThis.data.getOrElse(x, 0) +
                other.data.getOrElse(x, 0)))
      .toMap)
  }

  // equality
  override def equals(other: scala.Any): Boolean = {
    other match {
      case other: Multiset[A] =>
        downcast().data == other.downcast().data
      case _ => false
    }
  }
}

object Multiset {
  def apply[A](elements: A*): Multiset[A] =
    new Multiset[A](elements
      .groupBy(identity)
      .mapValues(_.length))

  def unapplySeq[A](multiset: Multiset[A]): Option[Seq[A]] =
    Some(multiset
      .data
      .flatMap(tuple => Seq.fill(tuple._2)(tuple._1))
      .toSeq)
}
