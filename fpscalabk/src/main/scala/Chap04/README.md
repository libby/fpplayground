## Handling Errors

## What FP doesn't like about Exceptions
* Exceptions break referential transparency and the substitution model.
* Exception require a knowledge of an extended context, they "make us context-depended."
* Not type safe.

## Possible Strategies besides Exceptions
1. C style error code. Not very descriptive.
2. 


## Total function vs Partial function
Partial function is when the function is not defined for all inputs, as in the
case where a function may throw an exception:

```scala
  
  def mean(xs: List[Double]): Double = xs.sum / xs.size
  
```
A Total function is defined for all possible inputs
```scala
  
  def mean(xs: List[Double]): Option[Double] = if (xs.size > 0) xs.sum/xs.size else None
```
> total funcition...it takes each value of the intput type to exactly one value of the output type.

## Benefits to mapping to Typed Error instead of a sentinel value, i.e. -999
1. obvious that there is a potential error type that may be returned.
1. the compiler can check that the caller of the function is handeling it correctly.

## Lifting functions
* `map` on Option *lifts* a function `A => B` to a function `Option[A] => Optoin[B]`
```scala

 def lift[A,B](f: A => B): Option[A] => Option[B] = _ map(f)

 val absO: Option[Double] = lift(math.abs)
```
lifting a function, i.e. math.abs into an Option allows us to work with it in an **Option context** after the fact.

## sequence and traverse
if any `Option[A] == None` None return `None`
```scala
  def sequence[A](a: List[Option[A]]): Option[List[A]]
```
if one fails return None
```scala
  def traverse[A,B](as: List[A])(f: A => Option[B]): Option[List[B]]
```

> between, map, lift, sequence, traverse, map2, map3 and so on, you should never have to modify any existing functions to work with optional values.

## Either, the more detailed error handling
> Represent in a very general way, values that can be on of two things. We can say that it's a *disjoint* union of two types.

> Representing effects as values.