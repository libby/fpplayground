import scala.annotation.tailrec

/**
 *  :load ../Chap02/mypolymodule.scala
 */
object mypolymodule {
    
    // tied to Array[String]
    def findFirstStr(ss: Array[String], key: String): Int = {
        @tailrec
        def inner(ss: Array[String], key: String, index: Int): Int = {
            if (index >= ss.size) -1 // don't throw exception, not the FP way. throw new Exception("key not found")
            else if (ss(index) == key) return index
            else inner(ss, key, index + 1)
        }
        
        inner(ss, key, 0)
    }
    
    // ploymorphic on type of array, but can only find first.
    def findFrist2[A](ss: Array[A], key: A): Int = {

        def inner[A](ss: Array[A], key: A, index: Int): Int = {
            if (index >= ss.size) -1
            else if (ss(index) == key) return index
            else inner(ss, key, index + 1)
        }
        
        inner(ss, key, 0)
    }
    
    // even better just test with a function
    def find[A](ss: Array[A], cond: A => Boolean): Int = {

        def inner(index: Int): Int = {
            if (index >= ss.size) -1
            else if (cond(ss(index))) return index
            else inner(index + 1)
        }
        
        inner(0)
    }
    
    def findFirst[A](ss: Array[A], key: A) = find(ss, { a: A => (a == key) })
    
    def isSorted(as: Array[Int]): Boolean = {
       
        def inner(a1: Int, a2: Int, index: Int): Boolean = {
            if (a1 > a2) false
            else if (index >= (as.size - 1)) true
            else inner(a2, as(index + 1), index + 1)
        }
        inner(as(0), as(1), 1)
    }
    
    // Final generic sort
    def isSortedGen[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {

        def inner(a1: A, a2: A, index: Int): Boolean = {
            if(!ordered(a1, a2)) false
            else if (index >= (as.size - 1)) true // the condition held all the way through
            else inner(a2, as(index + 1), index + 1)
        }
        inner(as(0), as(1), 1)
    }
    
    val lessThan = (x: Int, y: Int) => (x < y)
    
    def isSorted[A <: Ordered[A]](as: Array[A]): Boolean = isSortedGen(as, { (a1: A, a2: A) => a1 < a2 })
    
}
import mypolymodule._