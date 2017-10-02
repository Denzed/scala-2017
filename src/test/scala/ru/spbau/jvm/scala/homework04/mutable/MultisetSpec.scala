package ru.spbau.jvm.scala.homework04.mutable

import org.scalatest.{FlatSpec, MustMatchers}

class MultisetSpec extends FlatSpec with MustMatchers {
  "Multiset" should "add elements" in {
    val multiset = Multiset[Int]()

    multiset.add(1) must be ('empty)
    multiset.add(7) must be ('empty)
    multiset.add(7) must be (Some(1))
    multiset.add(9) must be ('empty)
  }

  "Multiset" should "find element" in {
    val multiset = Multiset(1, 2)

    multiset.get(1) must be (Some(1))
    multiset.get(2) must be (Some(2))
    multiset.get(3) must be ('empty)
  }

  "Multiset" should "be able to filter by predicate" in {
    val multiset = Multiset(1, 1, 7, 9, 9)

    multiset.filter(x => x > 5) must === (Multiset(7, 9, 9))
  }

  "Multiset" should "be able to apply function to its elements" in {
    val multiset = Multiset(1, 1, 7, 9, 9)

    multiset.map(x => x * 2) must === (Multiset(2, 2, 14, 18, 18))
  }

  "Multiset" should "be able to flatMap" in {
    val multiset = Multiset("a", "bb")

    multiset.flatMap(x => x * 2) must === (Multiset('a', 'a', 'b', 'b', 'b', 'b'))
  }

  "Multiset" should "be compatible with for-comprehension" in {
    val multiset = Multiset(1, 1, 7, 9, 9)
    val result = for {
      x <- multiset if x != 7
    } yield x * 2

    result must === (Multiset(2, 2, 18, 18))
  }

  "Multiset" should "be compatible with for-comprehension flatMapping" in {
    val result = for {
      outer <- Multiset(1, 2)
      inner <- List(2, 4)
    } yield outer * inner

    result must === (Multiset(2, 4, 4, 8))
  }

  "Multiset" should "support basic set operations" in {
    val setA = Multiset(1, 1, 7, 9, 9)
    val setB = Multiset(5, 5, 7, 7)

    setA & setB must === (Multiset(7))
    setA | setB must === (Multiset(1, 1, 5, 5, 7, 7, 7, 9, 9))
  }

  "Multiset" should "support pattern matching" in {
    val multiset = Multiset(1, 7, 7, 9)

    multiset match {
      case Multiset(x1, x2, x3, x4) => Multiset(x1, x2, x3, x4) must === (multiset)
      case _ => fail("However, it does not")
    }
  }
}
