## Chapter 3 - Functional data structures

* pattern matching
* writing and generalizing pure functions

> A functional data structure is operated on using only pure functions...(they) are by definition immutable

#### Requirements of a pure function
1. must not change data in place.
2. must not perform side effects.

* `singly linke list` most ubiquitous functional recursive data structure

### Variance with respect to data types
* `invariant` `List[A]` type param is fixed.
* `covarient` `List[+A]` then for type `X, Y` is `X` is a subclass of `Y` then `List[X]` is a subtype of `List[Y]`
* `contravarient`
* `variadic function` apply(as: A*) accepts 0 or more args of `type A` inside apply the elements will be bound to a `Seq`.
   the `-*` type annotation allows us to pass a `Seq` to a variadic method.

## Data sharing in functional data structures
* When added an element to the front of an existing list, i.e. 1 to xs, a new List is returned: 1 :: xs yields Cons(1, xs).
  Since Lists are immutable, there is no reason to make a copy and xs can be reused. This is *data sharing*
* When removing an element from a List the same is true, the `tail` can be returned as is and there is no need for modification.
* These are call `persistent data structures`

## Inferring element types i.e. 
```scala
 	dropWhile[A](l: List[A], p: Int => Boolean) 
	\\vs. 
	dropWhile[A](l: List[A])(p: Int => Boolean)
```
* currying a function allows for automatic type inference
> the main reason for grouping the arguments this way is to assist with type inference.

* When grouping arguments in a curried fashioned, the type flows from right to left, so the first argument fixes the type.

## ADT Algebraic Data Types (don't confuse with Abstract Data Type)
> An ADT is just a data type defined by one or more data constructors, each of which may contain zero or more arguments.

* `List` `Tree` as implemented in the chapter, expose multiple constructors that can be nested to form new structure.