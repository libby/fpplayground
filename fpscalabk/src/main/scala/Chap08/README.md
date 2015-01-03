## Property-based testing

### recap

> API should form an algebra - a collection of data types, functions over these data types, and importantly, laws or properties that express relationships between these functions. ... check these laws automatically.

> decouple the specification of program behavior from the creation of test cases. The programmer focuses on specifying the behavior of programs and giving high-level constraints on the test cases; the framework then automatically generates test

```scala
val intList: Gen[List[Int]] = Gen.listOf(Gen.choose(0,100))
val prop = forAll(intList) (ns => ns.reverse.reverse == ns) &&
	   forAll(intList) (ns => ns.headOption == ns.reverse.lastOption)
val failingProp = forAll(intList)(ns => ns.reverse == ns)

scala> prop.check 
```