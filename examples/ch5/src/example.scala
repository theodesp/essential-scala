object Example extends App {
  // Generic types allow us to abstract over types

  final case class Box[A](value: A)

  Box(2).value
  Box("hi").value

  def generic[A](in: A): A = in

  generic[String]("foo")
  generic(1)


  sealed trait Result[A]
  case class Success[A](result: A) extends Result[A]
  case class Failure[A](reason: String) extends Result[A]

  // 5.1.3 Exercises


  //sealed trait List[A] {
  //  def length: Int =
  //    this match {
  //      case End() => 0
  //      case Pair(_, tail) => 1 + tail.length
  //    }
  //
  //  def contains(item: A): Boolean =
  //    this match {
  //      case End() => false
  //      case Pair(hd, tail) => if (hd == item) true else tail.contains(item)
  //    }
  //
  //  def apply(index: Int): Result[A] =
  //    this match {
  //      case Pair(hd, tl) =>
  //        if (index == 0)
  //          Success(hd)
  //        else
  //          tl(index - 1)
  //      case End() =>
  //        Failure("Index out of bounds")
  //    }
  //}
  //final case class End[A]() extends List[A]
  //final case class Pair[A](head: A, tail: List[A]) extends List[A]

  //sealed trait IntList {
  //  def fold(end: Int, f: (Int, Int) => Int): Int =
  //    this match {
  //      case End => end
  //      case Pair(hd, tl) => f(hd, tl.fold(end, f))
  //    }
  //  def sum =
  //    foldGen[Int](0, (hd, tail) => hd + tail)
  //  def length: Int =
  //    foldGen[Int](0, (_, tl) => 1 + tl)
  //  def product: Int =
  //    foldGen[Int](1, (hd, tl) => hd * tl)
  //  def double: IntList =
  //    foldGen[IntList](End, (hd, tl) => Pair(hd * 2, tl))
  //
  //  def foldGen[A](end: A, f: (Int, A) => A): A =
  //    this match {
  //      case End => end
  //      case Pair(hd, tl) => f(hd, tl.fold(end, f))
  //    }
  //}
  //
  //case object End extends IntList
  //final case class Pair(head: Int, tail: IntList) extends IntList

  def example(x: Int)(y: Int) = x + y
  example(1)(2)

  sealed trait Tree[A] {
    def fold[B](node: (B, B) => B, leaf: A => B): B
  }
  final case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A] {
    def fold[B](node: (B, B) => B, leaf: A => B): B =
      node(left.fold(node, leaf), right.fold(node, leaf))
  }
  final case class Leaf[A](value: A) extends Tree[A] {
    def fold[B](node: (B, B) => B, leaf: A => B): B =
      leaf(value)
  }

  case class Pair[A, B](one: A, two: B)

  ("H1", 1)

  def tuplized[A, B](in: (A, B)) = in._1
  tuplized(("a", 1))

  (1, "a") match {
    case (a, b) => a + b
  }

  val x = (1, "b", true)
  x._1
  x._3

  sealed trait Sum[A, B] {
    def fold[C](left: A => C, right: B => C): C =
      this match {
        case Left(a) => left(a)
        case Right(a) => right(a)
      }
  }
  final case class Left[A, B](value: A) extends Sum[A, B]
  final case class Right[A, B](value: B) extends Sum[A, B]

  sealed trait Maybe[A] {
    def fold[B](empty: B, full: A => B): B =
      this match {
        case Full(v) => full(v)
        case Empty() => empty
      }
  }
  final case class Empty[A]() extends Maybe[A]
  final case class Full[A](value: A) extends Maybe[A]

  sealed trait LinkedList[A] {
    def map[B](fn: A => B): LinkedList[B] =
      this match {
        case Pair(hd, tl) => Pair(fn(hd), tl.map(fn))
        case End() => End[B]()
      }
  }
  final case class Pair[A](head: A, tail: LinkedList[A]) extends LinkedList[A]
  final case class End[A]() extends LinkedList[A]

  sealed trait Maybe[A] {
    def flatMap[B](fn: A => Maybe[B]): Maybe[B] =
      this match {
        case Full(v) => fn(v)
        case Empty() => Empty[B]()
      }
    // Implement map for Maybe.
    def map[B](fn: A => B): Maybe[B] =
      this match {
        case Full(v) => Full(fn(v))
        case Empty() => Empty[B]()
      }

    //For bonus points, implement map in terms of flatMap.
    def map2[B](fn: A => B): Maybe[B] =
      flatMap[B](v => Full(fn(v)))
  }
  final case class Full[A](value: A) extends Maybe[A]
  final case class Empty[A]() extends Maybe[A]

  // A type like F[A] with a map method is called a functor.
  // If a functor also has a flatMap method it is called a monad
  // The general idea is a monad represents a value in some context.
  // The context depends on the monad we’re using.
  // We use map when we want to transform the value within the context to a new
  // value, while keeping the context the same.
  // We use flatMap when we want to transform the value and provide a new context.

  // Given this list
  //
  //val list = List(1, 2, 3)
  //return a List[Int] containing both all the elements and their negation. Order is not important. Hint: Given an element create a list containing it and its negation.

  val list = List(1, 2, 3)
  list.flatMap(v => List(v, -v))

  sealed trait Sum[A, B] {
    def fold[C](error: A => C, success: B => C): C =
      this match {
        case Failure(v) => error(v)
        case Success(v) => success(v)
      }
    def map[C](f: B => C): Sum[A, C] =
      this match {
        case Failure(v) => Failure(v)
        case Success(v) => Success(f(v))
      }
    def flatMap[C](f: B => Sum[A, C]) =
      this match {
        case Failure(v) => Failure(v)
        case Success(v) => f(v)
      }
  }
  final case class Failure[A, B](value: A) extends Sum[A, B]
  final case class Success[A, B](value: B) extends Sum[A, B]

  // 5.6.1 Invariance, Covariance, and Contravariance
  // A type Foo[T] is invariant in terms of T,
  // meaning that the types Foo[A] and Foo[B] are unrelated
  // regardless of the relationship between A and B.
  // This is the default variance of any generic type in Scala.
  //
  //A type Foo[+T] is covariant in terms of T,
  // meaning that Foo[A] is a supertype of Foo[B] if A is a supertype of B.
  // Most Scala collection classes are covariant in terms of their contents.
  // We’ll see these next chapter.
  //
  //A type Foo[-T] is contravariant in terms of T, meaning that Foo[A] is a
  // subtype of Foo[B] if A is a supertype of B. The only example of
  // contravariance that I am aware of is function arguments.

  sealed trait Maybe[A]
  final case class Full[A](value: A) extends Maybe[A]
  case object Empty extends Maybe[Nothing]

  sealed trait Sum[+A, +B] {
    def fold[C](error: A => C, success: B => C): C =
      this match {
        case Failure(v) => error(v)
        case Success(v) => success(v)
      }
    def map[C](f: B => C): Sum[A, C] =
      this match {
        case Failure(v) => Failure(v)
        case Success(v) => Success(f(v))
      }
    def flatMap[AA >: A, C](f: B => Sum[AA, C]): Sum[AA, C] =
      this match {
        case Failure(v) => Failure(v)
        case Success(v) => f(v)
      }
  }
  final case class Failure[A](value: A) extends Sum[A, Nothing]
  final case class Success[B](value: B) extends Sum[Nothing, B]

  sealed trait Expression {
    def eval: Sum[String, Double] =
      this match {
        case Addition(l, r) => lift2(l, r, (left, right) => Success(left + right))
        case Subtraction(l, r) => lift2(l, r, (left, right) => Success(left - right))
        case Division(l, r) => lift2(l, r, (left, right) =>
          if(right == 0)
            Failure("Division by zero")
          else
            Success(left / right)
        )
        case SquareRoot(v) =>
          v.eval flatMap { value =>
            if(value < 0)
              Failure("Square root of negative number")
            else
              Success(Math.sqrt(value))
          }
        case Number(v) => Success(v)
      }

    def lift2(l: Expression, r: Expression, f: (Double, Double) => Sum[String, Double]) =
      l.eval.flatMap { left =>
        r.eval.flatMap { right =>
          f(left, right)
        }
      }
  }
  final case class Addition(left: Expression, right: Expression) extends Expression
  final case class Subtraction(left: Expression, right: Expression) extends Expression
  final case class Division(left: Expression, right: Expression) extends Expression
  final case class SquareRoot(value: Expression) extends Expression
  final case class Number(value: Int) extends Expression
}