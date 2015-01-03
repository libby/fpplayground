## Purely Functional Parallelism

#### Goal:

* create a purely functional library  for creating parallel and asynchronous computations.
* rein in complexity inherent in parallel programs by describing them using only pure functions.

#### Outcome:
* lets us use the substitution model to simplify our reasoning.

#### Focus on
* making libraries *highly composable and modular*
* keep in mind separating the concern of *describing* an operation from *running* it.
* when should things be lazy vs strict

* should evaluation be the resposibility of `fork` or `get` should it be eager or lazy?
* eval in get or run

### Lingo
* `derived combinator` vs `primitive combinator`
* lift function A => B to become a function that takes `Par[A]` and returns `Par[B]`
  we can map any function over Par

```scala

  type Par[A] = ExecutorService => Future[A]

  def map[A,B](pa: Par[A])(f: A => B): Par[B] = map2(pa, unit(()))((a,_) => f(a))

```

TODO
delve into ExecutorService

Hard jump for me thinking of representations like Par[A] as a function, the first function is just preparing the meal, the Par function cooks it.

## Thinking about laws

### *Law* - `map(unit(x)) () == unit (f(x))`
#### constraints
* can't make any assumptions or change behavior based on the values it receives.
* law disallows downcasting or `isInstanceOf` checks (typecasting) in implementations of map and unit.

```scala
// derive law

 map(unit(x)) (id) == unit(id(x))
 map(unit(x)) (id) == unit(x)
 map(y) (id) == y
````
### *Law* - `map(y) (id) == y` new law talks only about map
* it can't throw an exception and crash the computation before applying the function to the result
* all it can do is apply the function.
* map is said to be *structure-perserving*, doesn't alter the structure, only the internal value.

### *Law* `fork(x) == x` fork should not aff the result of a parallel computation.