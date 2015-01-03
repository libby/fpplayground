## Purely functional

## Random Number Generator
* pass state along
* repeatable 

### State Actions or State Transitions
* `state actions` or `state transitions` transform one state to the next `RNG => (A, RNG)`
* can be combined using `combinators`: higher-order functions that we.
* *Goal*: want `combinators` to pass the state along for us automatically.

> a *program* that depends on some RNG, uses it to generate an A, and also transitions the RNG to a new state that can be used by 
another action later.

```scala
 type Rand[+A] = RNG => (A, RNG)
 val int: Rand[Int] = _.nextInt
```

### Major functional players
* `unit`
* `map`
* `map2`
* `flatMap`
* `sequence`

* can generalize these guys
```scala
  def map[S,A,B](a: S => (A,S))(f: A => B): S => (B,S)

  // generalize the state from RNG
  type State[S,+A] = S => (A,S)
  case class State[S, +A](run: S => (A,S))
```
then 
```scala
  type Rand[A] = State[RNG, A]
```

## Key
> In the imperative programming paradigm, a program is a sequence of statements where each statement may modify the program state. That's exactly what we've been doing, except that our "statements" are really `State` action, which are really functions.