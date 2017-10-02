val i = 42

val anyVal = (i: AnyVal)

val value: AnyRef = new Object

def function1(int: Int): String = "" // Int => String

val function: Int => String = function1
function(42)

def function2(pair: (Int, String)): String = "" // (Int, String) => String

def function3(s: String)
             (int: Int = s.length) = s

val intToString: (Int) => String = function3("")
function3("")(5)

// Bar <: Foo
// immutable.List[Bar] <: immutable.List[Foo]

42 :: 42 :: 5 :: Nil

val ints: List[Any] = Nil.::(5)
  .::("")
  .::(42)
::(42, ::(42, ::(5, Nil)))
val ints1: List[Int] = List(42, 42, 5)

import scala.collection.mutable

mutable.Seq()
val seq = Seq(1, 2, 3)
seq.filter(_ != 2)
  .map(_ * 2)
  .flatMap(i => Seq(i, i + 1))
  .foreach(println)

for (i <- seq
     if i != 2;
     j = i * 2) {
  println(j)
}

val seq1: Seq[Int] = for (i <- seq
                          if i != 2;
                          j = i * 2)
  yield j

val maybeInt: Option[Int] = Some(4)
if (maybeInt.isDefined) println(maybeInt.get)

maybeInt match {
  case Some(i) => println(i)
  case None =>
}

maybeInt.foreach(println)

for (i <- maybeInt) {
  println(i)
}

for (i <- 0 until 10) {
  println(i)
}

(0 until 10).foreach(println)

case class Foo(s: String) {

  def map(f: String => Foo): Foo = Foo(s)

  def foreach(f: String => Unit): Unit = f(s)
}

for (foo <- Foo("hello!")) {
  println(foo)
}

(Some(3), Some(true)) match {
  case (Some(i), Some(b)) => Some(i, b)
  case _ => None
}

Some(3).flatMap { i =>
  Some(true).map((i, _))
}

val maybeTuple = for {
  i <- Some(3)
  flag <- Some(true)
} yield (i, flag)