Chapter-1
----

```bash
➜ scala
Welcome to Scala 2.12.8 (Java HotSpot(TM) 64-Bit Server VM, Java 1.8.0_162).
Type in expressions for evaluation. Or try :help.

scala> "hello world"
res0: String = hello world

scala> 1+2+3
res1: Int = 6

scala> res1 * 2
res2: Int = 12

scala> println("Hello world!")
Hello world!

// Multiline
#
scala> for (i <- 1 to 3) {
     | println(i)
     | }
1
2
3

// Pasting
scala> :paste
// Entering paste mode (ctrl-D to finish)

val x = 1
val y = 2
x + y


// Exiting paste mode, now interpreting.

x: Int = 1
y: Int = 2
res4: Int = 3

// Paste from file
scala> :paste example.scala
Pasting file example.scala...
x: Int = 1
y: Int = 2
res0: Int = 3

// Type check
scala> :type println("Hello world!")
Unit

```
