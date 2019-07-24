object example extends App {
  import scala.runtime.Nothing$

  class Person3 {
    val firstName = "Noel"
    val lastName = "Welsh"
    def name: String = firstName + " " + lastName
  }

  val noel = new Person3
  noel.name

  object alien {
    def greet(p: Person3) =
      "Greetings, " + p.firstName + " " + p.lastName
  }

  alien.greet(noel)

  class Person2(val firstName: String, val lastName: String) {
    def name = firstName + " " + lastName
  }

  new Person2("Dave", "Gurnell").firstName

  def badness = throw new Exception("Error")
  def otherbadness = null

  // 3.1.6 Exercises
  case class Cat(colour: String, food: String)

  object ChipShop {
    def willServe(cat: Cat) =
      if (cat.food == "Chips")
        true
      else
        false
  }

  class Director(
                  val firstName: String,
                  val lastName: String,
                  val yearOfBirth: Int) {

    def name: String =
      s"$firstName $lastName"

    def copy(
              firstName: String = this.firstName,
              lastName: String = this.lastName,
              yearOfBirth: Int = this.yearOfBirth): Director =
      new Director(firstName, lastName, yearOfBirth)
  }

  class Film(
              val name: String,
              val yearOfRelease: Int,
              val imdbRating: Double,
              val director: Director) {

    def directorsAge =
      yearOfRelease - director.yearOfBirth

    def isDirectedBy(director: Director) =
      this.director == director

    def copy(
              name: String = this.name,
              yearOfRelease: Int = this.yearOfRelease,
              imdbRating: Double = this.imdbRating,
              director: Director = this.director): Film =
      new Film(name, yearOfRelease, imdbRating, director)
  }

  class Adder(amount: Int) {
    def add(in: Int) = in + amount
  }

  class Counter(val count: Int) {
    def dec: Counter = dec()
    def inc: Counter = inc()
    def dec(amount: Int = 1): Counter = new Counter(count - amount)
    def inc(amount: Int = 1): Counter = new Counter(count + amount)
    def adjust(adder: Adder) = new Counter(adder.add(count))
  }

  new Counter(0).dec.inc.dec.dec

  class Adder2(amount: Int) {
    def apply(in: Int): Int = in + amount
  }

  val add3 = new Adder2(3)
  add3(4)

  //As we saw earlier, Scala has two namespaces: a space of type names and a space of value names. This separation allows us to name our class and companion object the same thing without conflict.
  // It is important to note that the companion object is not an instance of the class—it is a singleton object with its own type:
  // Companion objects replace Java’s static methods.

  object Person {
    def apply(fullName: String) = {
      val parts = fullName.split(" ")
      new Person2(parts.head, parts.last)
    }
  }

  Person("Hello Despoudis")

  object Director {
    def apply(firstName: String, lastName: String, yearOfBirth: Int): Director =
      new Director(firstName, lastName, yearOfBirth)

    def older(director1: Director, director2: Director): Director =
      if (director1.yearOfBirth < director2.yearOfBirth) director1 else director2
  }

  object Film {
    def apply(
               name: String,
               yearOfRelease: Int,
               imdbRating: Double,
               director: Director): Film =
      new Film(name, yearOfRelease, imdbRating, director)

    def newer(film1: Film, film2: Film): Film =
      if (film1.yearOfRelease < film2.yearOfRelease) film1 else film2

    def highestRating(film1: Film, film2: Film): Double = {
      val rating1 = film1.imdbRating
      val rating2 = film2.imdbRating
      if (rating1 > rating2) rating1 else rating2
    }

    def oldestDirectorAtTheTime(film1: Film, film2: Film): Director =
      if (film1.directorsAge > film2.directorsAge) film1.director else film2.director
  }

  // A final note. If you find yourself defining a case class with no constructor arguments you can instead a define a case object.

  case object Citizen {
    def firstName = "John"
    def lastName  = "Doe"
    def name = firstName + " " + lastName
  }

  case class Person(firstName: String, lastName: String)

  object Stormtrooper {
    def inspect(person: Person): String =
      person match {
        case Person("Luke", "Skywalker") => "Stop, rebel scum!"
        case Person("Han", "Solo") => "Stop, rebel scum!"
        case Person(first, last) => s"Move along, $first"
      }
  }

  // 3.5.3 Exercises

  object ChipShoppe {
    def willServe(cat: Cat) =
      cat match {
        case Cat(_, "Chips") => true
        case Cat(_, _) => false
      }
  }

  //object Dad {
  //  def rate(film: Film): Double =
  //    film match {
  //      case Film(_, _, _, Director("Clint", "Eastwood", _)) => 10.0
  //      case Film(_, _, _, Director("John", "McTiernan", _)) => 7.0
  //      case _ => 3.0
  //    }
  //}
}