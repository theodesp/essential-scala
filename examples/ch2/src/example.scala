object example extends App {
  2.min(3)
  2 min 3

  "3".toInt // Int = 3
  //"foo".toInt // java.lang.NumberFormatException: For input string: "foo"

  "hello".toUpperCase

  "abcdef".take(3)
  "abcdef".take(2)

  123.toShort
  123.toByte
  "the quick brown fox" split " "

  2 * 3 + 4 * 5
  (2 * 3) + (4 * 5)
  2 * (3 + 4) * 5

  //2.2.5 Exercises
  "foo" take 1
  1.+(2).+(3)

  ()
  println("something")

  // 2.3.8 Exercises
  'a' //char
  "a" //string

  "Hello world!" //string

  println("Hello world!") // Unit

  // 'Hello world!'

  object Test {}

  object Test2 {
    def name: String = "Probably the best object ever"
  }

  Test2.name

  object Test3 {
    def hello(name: String) =
      "Hello " + name
  }

  Test3.hello("Noel")

  object Test4 {
    val name = "Noel"
    def hello(other: String): String =
      name + " says hi to " + other
  }

  Test4.hello("Dave")

  object Test7 {
    val simpleField = {
      println("Evaluating simpleField")
      42
    }
    def noParameterMethod = {
      println("Evaluating noParameterMethod")
      42
    }
  }

  Test7
  Test7.noParameterMethod
  Test7.noParameterMethod

  // 2.4.5 Exercises

  object Oswald {
    val Colour = "Black"
    val Food = "Milk"
  }

  object Calc {
    def square(a: Double): Double = a * a
    def cube(a: Double): Double = a * square(a)
  }

  object Calc2 {
    def square(a: Int): Int = a * a
    def cube(a: Int): Int = a * square(a)
  }

  val x: Int = Calc.square(2).toInt

  object argh {
    def a = {
      println("a")
      1
    }

    val b = {
      println("b")
      a + 2
    }

    def c = {
      println("c")
      a
      b + "c"
    }
  }

  argh.c + argh.b + argh.a

  object person {
    val firstName = "Dave"
    val lastName = "Gurnell"
  }

  object alien {
    def greet(p: person.type) =
      "Greetings, " + p.firstName + " " + p.lastName
  }

  alien.greet(person)

  object clock {
    def time = System.currentTimeMillis
  }
  clock.time
  clock.time
}
