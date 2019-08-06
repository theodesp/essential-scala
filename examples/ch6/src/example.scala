object ch6 extends App {
  val sequence = Seq(1,2,3)
  0 +: sequence
  sequence.map(elt => elt * 2)
  sequence.map(_ * 2)

  val list = 1 :: 2 :: 3 :: Nil
  List(1, 2, 3) ::: List(4, 5, 6)
  list.size

  val animals = Seq("cat", "dog")
  animals :+ "tyrannosaurus"
  "mouse" +: animals
  2 +: animals

  "dog".toSeq.permutations
  Seq(1, 2, 3).flatMap(num => Seq(num, num * 10))
  Seq(1, 2, 3).foldLeft(0)(_ + _) // (((0 + 1) + 2) + 3)
  Seq(1, 2, 3).foldRight(0)(_ + _) // (1 + (2 + (3 + 0)))

  def readInt(str: String): Option[Int] =
    if(str matches "-?\\d+") Some(str.toInt) else None

  Seq(readInt("1"), readInt("b"), readInt("3")).flatMap(x => x)

  val optionA = readInt("123")
  val optionB = readInt("234")

  for {
    a <- optionA
    b <- optionB
  } yield a + b

  def addOptions(opt1: Option[Int], opt2: Option[Int]) =
    for {
      a <- opt1
      b <- opt2
    } yield a + b

  def addOptions2(opt1: Option[Int], opt2: Option[Int]) =
    opt1 flatMap { a =>
      opt2 map { b =>
        a + b
      }
    }

  def divide(numerator: Int, denominator: Int) =
    if(denominator == 0) None else Some(numerator / denominator)

  def divideOptions(numerator: Option[Int], denominator: Option[Int]) =
    for {
      a <- numerator
      b <- denominator
      c <- divide(a, b)
    } yield c

  def calculator(operand1: String, operator: String, operand2: String): Unit = {
    val result = for {
      a   <- readInt(operand1)
      b   <- readInt(operand2)
      ans <- operator match {
        case "+" => Some(a + b)
        case "-" => Some(a - b)
        case "*" => Some(a * b)
        case "/" => divide(a, b)
        case _   => None
      }
    } yield ans

    result match {
      case Some(number) => println(s"The answer is $number!")
      case None         => println(s"Error calculating $operand1 $operator $operand2")
    }
  }

  Seq(1, 2, 3).zip(Seq(4, 5, 6))

  for(x <- Seq(1, 2, 3).zip(Seq(4, 5, 6))) yield { val (a, b) = x; a + b }
  for(x <- Seq(1, 2, 3).zipWithIndex) yield x
  for((a, b) <- Seq(1, 2, 3).zip(Seq(4, 5, 6))) yield a + b

  for {
    x     <- Seq(1, 2, 3)
    square = x * x
    y     <- Seq(4, 5, 6)
  } yield square * y

  val example = Map("a" -> 1, "b" -> 2, "c" -> 3)
  scala.collection.immutable.ListMap("a" -> 1) + ("b" -> 2) + ("c" -> 3) + ("d" -> 4) + ("e" -> 5)

  1 until 10
  10 until 1
  10 until 1 by -1

}