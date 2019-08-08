object ch7 extends App {
  import scala.math.Ordering
  val minOrdering = Ordering.fromLessThan[Int](_ < _)
  // minOrdering: scala.math.Ordering[Int] = scala.math.Ordering$$anon$9@63a106c2

  val maxOrdering = Ordering.fromLessThan[Int](_ > _)
  // maxOrdering: scala.math.Ordering[Int] = scala.math.Ordering$$anon$9@3e8635b0

  List(3, 4, 2).sorted(minOrdering)
  // res0: List[Int] = List(2, 3, 4)

  List(3, 4, 2).sorted(maxOrdering)
  // res1: List[Int] = List(4, 3, 2)
  implicit val ordering = Ordering.fromLessThan[Int](_ < _)

  List(2, 4, 3).sorted
  // res2: List[Int] = List(2, 3, 4)

  List(1, 7 ,5).sorted
  // res3: List[Int] = List(1, 5, 7)

  val absOrdering = Ordering.fromLessThan[Int]{ (x, y) =>
    Math.abs(x) < Math.abs(y)
  }

  implicit val absOrdering = Ordering.fromLessThan[Int]{ (x, y) =>
    Math.abs(x) < Math.abs(y)
  }

  final case class Rational(numerator: Int, denominator: Int)

  implicit val ordering = Ordering.fromLessThan[Rational]((x, y) =>
    (x.numerator.toDouble / x.denominator.toDouble) <
      (y.numerator.toDouble / y.denominator.toDouble)
  )

  object RationalLessThanOrdering {
    implicit val ordering = Ordering.fromLessThan[Rational]((x, y) =>
      (x.numerator.toDouble / x.denominator.toDouble) <
        (y.numerator.toDouble / y.denominator.toDouble)
    )
  }

  object RationalGreaterThanOrdering {
    implicit val ordering = Ordering.fromLessThan[Rational]((x, y) =>
      (x.numerator.toDouble / x.denominator.toDouble) >
        (y.numerator.toDouble / y.denominator.toDouble)
    )
  }

  final case class Order(units: Int, unitPrice: Double) {
    val totalPrice: Double = units * unitPrice
  }

  object Order {
    implicit val lessThanOrdering = Ordering.fromLessThan[Order]{ (x, y) =>
      x.totalPrice < y.totalPrice
    }
  }

  object OrderUnitPriceOrdering {
    implicit val unitPriceOrdering = Ordering.fromLessThan[Order]{ (x, y) =>
      x.unitPrice < y.unitPrice
    }
  }

  object OrderUnitsOrdering {
    implicit val unitsOrdering = Ordering.fromLessThan[Order]{ (x, y) =>
      x.units < y.units
    }
  }

  trait Equal[A] {
    def equal(v1: A, v2: A): Boolean
  }
  case class Person(name: String, email: String)

  object EmailEqual extends Equal[Person] {
    def equal(v1: Person, v2: Person): Boolean =
      v1.email == v2.email
  }

  object NameEmailEqual extends Equal[Person] {
    def equal(v1: Person, v2: Person): Boolean =
      v1.email == v2.email && v1.name == v2.name
  }

  object Eq {
    def apply[A](v1: A, v2: A)(implicit equal: Equal[A]): Boolean =
      equal.equal(v1, v2)
  }

  object NameAndEmailImplicit {
    implicit object NameEmailEqual extends Equal[Person] {
      def equal(v1: Person, v2: Person): Boolean =
        v1.email == v2.email && v1.name == v2.name
    }
  }

  object EmailImplicit {
    implicit object EmailEqual extends Equal[Person] {
      def equal(v1: Person, v2: Person): Boolean =
        v1.email == v2.email
    }
  }

  implicit class ExtraStringMethods(str: String) {
    val vowels = Seq('a', 'e', 'i', 'o', 'u')

    def numberOfVowels =
      str.toList.filter(vowels contains _).length
  }

  "the quick brown fox".numberOfVowels


  object Examples {
    def byNameAndEmail = {
      import NameAndEmailImplicit._
      Eq(Person("Noel", "noel@example.com"), Person("Noel", "noel@example.com"))
    }

    def byEmail = {
      import EmailImplicit._
      Eq(Person("Noel", "noel@example.com"), Person("Dave", "noel@example.com"))
    }
  }

  implicit class IntOps(n: Int) {
    def yeah() = for{ _ <- 0 until n } println("Oh yeah!")
  }

  2.yeah()

  object IntImplicits {
    implicit class IntOps(n: Int) {
      def yeah() =
        times(_ => println("Oh yeah!"))

      def times(func: Int => Unit) =
        for(i <- 0 until n) func(i)
    }
  }

  case class Example(name: String)
  implicit val implicitExample = Example("implicit")

  implicitly[Example]
  // res0: Example = Example(implicit)

  implicitly[Example] == implicitExample
  // res1: Boolean = true
}