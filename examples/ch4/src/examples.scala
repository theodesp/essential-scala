object example extends App {
  import java.util.Date

  sealed trait Visitor {
    def id: String      // Unique id assigned to each user
    def createdAt: Date // Date this user first visited the site

    // How long has this visitor been around?
    def age: Long = new Date().getTime - createdAt.getTime
  }

  final case class Anonymous(
                              id: String,
                              createdAt: Date = new Date()
                            ) extends Visitor

  final case class User(
                         id: String,
                         email: String,
                         createdAt: Date = new Date()
                       ) extends Visitor

  trait Feline {
    def colour: String
    def sound: String
  }

  case class Lion(colour: String, maneSize: Int) extends Feline {
    val sound = "roar"
  }

  case class Tiger(colour: String) extends Feline {
    val sound = "roar"
  }

  case class Panther(colour: String) extends Feline {
    val sound = "roar"
  }

  case class Cat(colour: String, food: String) extends Feline {
    val sound = "meow"
  }

  sealed trait Shape {
    def sides: Int
    def perimeter: Double
    def area: Double
  }

  object Draw {
    def apply(shape: Shape) =
      shape match {
        case Rectangle(width, height) =>
          s"A rectangle of width ${width}cm and height ${height}cm"

        case Square(size) =>
          s"A square of size ${size}cm"

        case Circle(radius) =>
          s"A circle of radius ${radius}cm"
      }
  }

  case class Circle(radius: Double) extends Shape {
    val sides = 1
    val perimeter = 2 * math.Pi * radius
    val area = math.Pi * radius * radius
  }

  sealed trait Rectangular extends Shape {
    def width: Double
    def height: Double
    val sides = 4
    override val perimeter = 2*width + 2*height
    override val area = width*height
  }

  case class Square(size: Double) extends Rectangular {
    val width = size
    val height = size
  }

  case class Rectangle(
                        val width: Double,
                        val height: Double
                      ) extends Rectangular

  // 4.2.2 Exercises
  sealed trait Color {
    def red: Double
    def green: Double
    def blue: Double

    def isLight = (red + green + blue) / 3.0 > 0.5
    def isDark = !isLight
  }

  case object Red extends Color {
    val red = 1.0
    val green = 0
    val blue = 0
  }

  case object Yellow extends Color {
    // Here we have implemented the RGB values as `vals`
    // because the values cannot change:
    val red = 1.0
    val green = 1.0
    val blue = 0.0
  }

  case object Pink extends Color {
    // Here we have implemented the RGB values as `vals`
    // because the values cannot change:
    val red = 1.0
    val green = 0.0
    val blue = 1.0
  }

  // The arguments to the case class here generate `val` declarations
  // that implement the RGB methods from `Color`:
  final case class CustomColor(
                                red: Double,
                                green: Double,
                                blue: Double) extends Color
  sealed trait DivisionResult
  final case class Finite(value: Int) extends DivisionResult
  case object Infinite extends DivisionResult

  object divide {
    def apply(num: Int, den: Int): DivisionResult =
      if(den == 0) Infinite else Finite(num / den)
  }

  divide(1, 0) match {
    case Finite(value) => s"It's finite: ${value}"
    case Infinite      => s"It's infinite"
  }
  // res34: String = It's infinite


  //// 4.4.4 Exercises
  //sealed trait TrafficLight
  //case object Red extends TrafficLight
  //case object Green extends TrafficLight
  //case object Yellow extends TrafficLight

  sealed trait Calculation
  final case class Success(result: Int) extends Calculation
  final case class Failure(reason: String) extends Calculation

  sealed trait Source
  case object Well extends Source
  case object Spring extends Source
  case object Tap extends Source
  final case class BottledWater(size: Int, source: Source, carbonated: Boolean)

  object Calculator {
    def +(calc: Calculation, operand: Int): Calculation =
      calc match {
        case Success(result) => Success(result + operand)
        case Failure(reason) => Failure(reason)
      }
    def -(calc: Calculation, operand: Int): Calculation =
      calc match {
        case Success(result) => Success(result - operand)
        case Failure(reason) => Failure(reason)
      }

    def /(calc: Calculation, operand: Int): Calculation =
      calc match {
        case Success(result) =>
          operand match {
            case 0 => Failure("Division by zero")
            case _ => Success(result / operand)
          }
        case Failure(reason) => Failure(reason)
      }
  }

  sealed trait IntList {
    def length: Int =
      this match {
        case End => 0
        case Pair(_, tl) => 1 + tl.length
      }

    def product: Int =
      this match {
        case End => 1
        case Pair(hd, tl) => hd * tl.product
      }

    def double: IntList =
      this match {
        case End => End
        case Pair(hd, tl) => Pair(hd * 2, tl.double)
      }
  }
  case object End extends IntList
  final case class Pair(head: Int, tail: IntList) extends IntList

  sealed trait Tree
  final case class Node(l: Tree, r: Tree) extends Tree
  final case class Leaf(elt: Int) extends Tree

  object TreeOps {
    def sum(tree: Tree): Int =
      tree match {
        case Leaf(elt) => elt
        case Node(l, r) => sum(l) + sum(r)
      }

    def double(tree: Tree): Tree =
      tree match {
        case Leaf(elt) => Leaf(elt * 2)
        case Node(l, r) => Node(double(l), double(r))
      }
  }

  // model JSON
  // Now add a method to convert your JSON representation to a String
  object json {
    sealed trait Json {
      def print: String = {
        def quote(s: String): String =
          '"'.toString ++ s ++ '"'.toString
        def seqToJson(seq: SeqCell): String =
          seq match {
            case SeqCell(h, t @ SeqCell(_, _)) =>
              s"${h.print}, ${seqToJson(t)}"
            case SeqCell(h, SeqEnd) => h.print
          }

        def objectToJson(obj: ObjectCell): String =
          obj match {
            case ObjectCell(k, v, t @ ObjectCell(_, _, _)) =>
              s"${quote(k)}: ${v.print}, ${objectToJson(t)}"
            case ObjectCell(k, v, ObjectEnd) =>
              s"${quote(k)}: ${v.print}"
          }

        this match {
          case JsNumber(v) => v.toString
          case JsString(v) => quote(v)
          case JsBoolean(v) => v.toString
          case JsNull => "null"
          case s @ SeqCell(_, _) => "[" ++ seqToJson(s) ++ "]"
          case SeqEnd => "[]"
          case o @ ObjectCell(_, _, _) => "{" ++ objectToJson(o) ++ "}"
          case ObjectEnd => "{}"
        }
      }
      final case class JsNumber(value: Double) extends Json
      final case class JsString(value: String) extends Json
      final case class JsBoolean(value: Boolean) extends Json
      case object JsNull extends Json
      sealed trait JsSequence extends Json
      final case class SeqCell(head: Json, tail: JsSequence) extends JsSequence
      case object SeqEnd extends JsSequence
      sealed trait JsObject extends Json
      final case class ObjectCell(key: String, value: Json, tail: JsObject) extends JsObject
      case object ObjectEnd extends JsObject
    }
}
