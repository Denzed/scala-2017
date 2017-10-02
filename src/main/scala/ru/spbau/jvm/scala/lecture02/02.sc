trait Token {
  val operation: Char
}

case object Plus extends Token {
  override val operation = '+'
}

case object Minus extends Token {
  override val operation = '-'
}

val token = new Token {
  override val operation: Char = '?'
}

token match {
  case Plus => println("1")
  case Minus => println("2")
  case _ => println("3")
}

sealed trait Tree {
  def isTree: Boolean = true
}

case object Leaf extends Tree

final case class Node(left: Option[Tree], right: Option[Tree]) extends Tree

class SimpleNode(val left: Tree, val right: Tree) extends Tree {

  def canEqual(other: Any): Boolean = other.isInstanceOf[SimpleNode]

  override def equals(other: Any): Boolean = other match {
    case that@SimpleNode(thatLeft, thatRight) =>
      (that canEqual this) &&
        left == thatLeft &&
        right == thatRight
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(left, right)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  override def toString = s"SimpleNode($left, $right)"
}

object SimpleNode {

  def apply(left: Tree, right: Tree = null): SimpleNode =
    (left, right) match {
      case (null, null) => throw new IllegalArgumentException
      case _ => new SimpleNode(left, right)
    }

  def unapply(node: SimpleNode): Option[(Tree, Tree)] =
    Some((node.left, node.right))
}

//val node = Node(null, null)
//val simpleNode = SimpleNode(null)

val tree: Tree = Leaf
tree match {
  case Leaf =>
  case Node(Some(left), Some(right)) =>
  case node: SimpleNode if node.left == Leaf =>
  case node: SimpleNode => // if (tree.isInstanceOf[SimpleNode])
    println(node.left)
  case SimpleNode(left, _) =>
    println(left)
}

val str = "text" + "text"
val str2 = ""
"String($str)"
s"String(${str2 + str2})"

str match {
  case null =>
  case "" =>
  case "texttext" => println(true)
}

val function: Int => String =
  _.toString

val function2: Function1[Int, String] =
  _.toString

function(0)

Node(None, None)
val function3: (Option[Node], Option[Node]) => Node = {
  case (maybeLeft@Some(left), maybeRight@Some(_)) if left.isTree => Node(maybeLeft, maybeRight)
}

def function4 = (_: Tree).isTree

def function5 = (tree: Tree).isTree && !tree.isTree
