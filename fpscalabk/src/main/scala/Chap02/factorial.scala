import scala.annotation.tailrec

object Factorial {
    
    /**
     * n * n-1 * n-2 ... 0
     * @param n number to get the factorial for.
     * @return the factorial for n.
     */
    def fact(n: Int): Int = {
        n match {
            case x: Int if x <= 1 => return x
            case x => x * fact(x - 1)
        }  
    }

    @tailrec
    def factTail(n: Int, acc: Int = 1): Int = {
      n match {
        case x: Int if x <= 1 => return acc
        case x => factTail(x - 1, x * acc)    
       }
    }
    
    @tailrec
    def factTail2(n: Int, acc: Int = 1): Int = {
        if (n <= 1) acc
        else factTail2(n - 1, n * acc)
    }
    
}