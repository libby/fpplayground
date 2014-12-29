import scala.annotation.tailrec

object Fibonacci {
    
    /**
     * 1 1 2 3 5
     * first + next
     */
    //@tailrec
    def nth(n: Int): Int = {
        
        def fib(n: Int, prev: Int, next: Int): Int = {
            if (n <= 1) next
            else fib(n - 1, next, prev + next)
        }
        fib(n, 0, 1)
    }
    
    /**
     * really slows down around 41
     */
    def fibClassic(n: Int): Int = {
        if (n <= 1) n
        else fibClassic(n-1) + fibClassic(n-2)
    }
    
}