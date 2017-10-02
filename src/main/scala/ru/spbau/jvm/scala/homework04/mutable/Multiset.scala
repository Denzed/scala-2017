package ru.spbau.jvm.scala.homework04.mutable

import scala.collection.{GenTraversableOnce, mutable}

class Multiset[A] private (val counter: mutable.Map[A,Int] = mutable.Map.empty) {
  // adds an element
  def add(element: A): Option[Int] =
    counter.put(element, counter.getOrElse(element, 0) + 1)

  // determines whether given element is contained in the multiset
  def apply(element: A): Boolean =
    counter.contains(element)

  // just for testing purposes
  private[mutable] def getCount(element: A): Option[Int] =
    counter.get(element)

  // get the element
  def get(element: A): Option[A] =
    counter.get(element) match {
      case Some(0) => Option.empty
      case Some(_) => Some(element)
      case _       => Option.empty
    }

  // filters elements by predicate
  def filter(p: (A) ⇒ Boolean): Multiset[A] =
    new Multiset(mutable.Map() ++= counter.filterKeys(p))

  def withFilter(p: (A) => Boolean): Multiset[A] = filter(p)

  // applies a given function to each element
  def map[B](f: (A) ⇒ B): Multiset[B] =
    new Multiset(mutable.Map() ++= counter
      .groupBy(tuple => f(tuple._1))
      .mapValues(_.foldLeft(0)((a, tuple) => a + tuple._2)))

  // maps each element to a some Traversable container and joins
  def flatMap[B](f: (A) ⇒ GenTraversableOnce[B]): Multiset[B] = {
    def helper(tuple: (A, Int)): Multiset[B] =
      new Multiset[B](mutable.Map() ++=
        f(tuple._1)
        .seq.toSeq
        .groupBy(identity)
        .mapValues(_.length * tuple._2))
    counter.foldLeft(Multiset[B]())((a, tuple) => a | helper(tuple))
  }

  // multiset intersection
  def &(other: Multiset[A]): Multiset[A] = {
    new Multiset(for {
      (x, cnt) <- counter if other.counter.keySet(x)
    } yield (x, Math.min(cnt, other.counter(x))))
  }

  // multiset union
  def |(other: Multiset[A]): Multiset[A] = {
    new Multiset(mutable.Map() ++= (for {
      x <- counter.keySet | other.counter.keySet
    } yield (x, counter.getOrElse(x, 0) +
      other.counter.getOrElse(x, 0))))
  }

  // equality
  override def equals(other: scala.Any): Boolean = {
    other match {
      case other: Multiset[A] =>
        counter == other.counter
      case _ => false
    }
  }
}

object Multiset {
  def apply[A](elements: A*): Multiset[A] =
    new Multiset(mutable.Map() ++= elements
      .groupBy(identity)
      .mapValues(_.length))

  def unapplySeq[A](multiset: Multiset[A]): Option[Seq[A]] =
    Some(multiset
      .counter
      .flatMap(tuple => Seq.fill(tuple._2)(tuple._1))
      .toSeq)
}
