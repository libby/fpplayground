object partialModule {
    
    /**
     * takes a value and a function of two args, and returns a function of one 
     * argument as its result.
     * HOF that takes a function of two arguments and partially applies it.
     */
    def partial1[A,B,C](a: A, f: (A,B) => C): B => C = { b => f(a, b) }
    
    def curry[A,B,C](f: (A, B) => C): A => (B => C) = { a => b => f(a, b) } 
    
    def uncurry[A,B,C](f: A => B => C): (A,B) => C = { (a,b) => f(a)(b) }
    
    // feed output of one function ot the input of another function.
    // f compose g
    def compose[A,B,C](f: B => C, g: A => B ): A => C = { a =>  f(g(a)) }
    
}
import partialModule._