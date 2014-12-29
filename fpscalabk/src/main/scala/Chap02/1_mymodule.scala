import scala.annotation.tailrec

object mymodule {
    
    def abs(n: Int): Int = {
        if (n < 0) n * -1
        else n
    }
    
    /**
     * HOF (Higher Order Function) monomorphic function, only takes one type Int => Int t
     * the types are bounded.
     * 
     * @param msg i.e. 'The absolute value' or ' The factorial value'
     * @param n Int the number to preform the operation on
     * @param Int => Int function to apply to n.
     * @return the string message with the result of apply f(n)
     */
    def formatValue(msg: String, n: Int, f: Int => Int): String = s"$msg of $n is ${f(n)}"
    
    @tailrec
    def factTail2(n: Int, acc: Int = 1): Int = {
       if (n <= 1) acc
       else factTail2(n - 1, n * acc)
    }
    
    def fact(n: Int): Int = {

       def inner(n: Int, acc: Int) = {    
           if (n <= 1) acc
           else factTail2(n - 1, n * acc)
       }
       inner(n, 1)
    }
    
    def formatAbs(n: Int) = formatValue("The absolute value", n, abs)
    def formatFact(n: Int) = formatValue("The factorial value", n, fact)
    
}