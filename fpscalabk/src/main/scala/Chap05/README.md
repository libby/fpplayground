## Strictness and Laziness

### Fusing sequences of transformations
* lazy lists

> Non-strictness is a property of a function. To say a function is non-strict just means that the function may choose not to evaluate one or more of is arguments. 

> a strict function always evaluates its arguments.

* `||` and `&&` short circuit boolean functions that are **non-strict** they may choose to evaluate only one of their arguments.
```scala

 def if2[A](cond: Boolean, onTrue: () => A, onFalse: () => A): A =
	if (cond) onTrue() else onFalse()
 
 // equivalent to 
 def if2[A](cond: Boolean, onTrue: => A, onFalse: => A): A =
	if (cond) onTrue else onFalse
	
```

* a **thunk** is the unevaluated form of an expression.
*