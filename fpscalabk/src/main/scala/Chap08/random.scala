package fpscala

sealed trait RNG {
    def nextInt: (Int, RNG)    
}

case class SimpleRNG(seed: Long) extends RNG {

    def nextInt: (Int, RNG) = {
        val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
        val nextRNG = SimpleRNG(newSeed)
        val n = (newSeed >>> 16).toInt
        (n, nextRNG)
    }
    
}

object RNG {
    
    type Rand[+A] = RNG => (A, RNG)
    
    val int: Rand[Int] = _.nextInt
    
    // always return a constant value.
    def unit[A](a: A): Rand[A] = rng => (a, rng)
    
    // transforming the output of a state action without modifying the state itself.
    def map[A,B](s: Rand[A])(f: A => B): Rand[B] = {
        rnd => 
            val (a, rnd2) = s(rnd)
            (f(a), rnd2)
    }
    
    // map2 know to look inside ra and rb to get the next state.
    /**
     * @param ra RNG => (A,RNG) function that will return the next value and the next state for A.
     * @param rb RNG => (B,RNG) function that will return the next value and the next state for B.
     * @param f function for combining the VALUES of ra and rb.
     * @return a function that given an RNG will compute the next value and next RNG (State).
     */
    def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] = {
        rng => 
            val (a, rng1) = ra(rng)
            val (b, rng2) = rb(rng1)
            (f(a,b), rng2)
    }
    
    // allows us to chain things together, pass along state.
    def flatMap[A,B](ra: Rand[A])(f: A => Rand[B]): Rand[B] = {
        rng =>
            val (a, nextRng) = ra(rng) // under the covers, get the next state and pass it along.
            f(a)(nextRng) // ah ha, so here we pass the state along
    }
    
    // exercise 6.8 map and map via flatMap
    def mapViaFM[A,B](ra: Rand[A])(f: A => B): Rand[B] = {
        flatMap(ra)({ a => unit(f(a)) })
    }
    
    def map2ViaFM[A,B,C](ra: Rand[A], rb: Rand[B])( f: (A,B) => C ): Rand[C] = {
        flatMap(ra)( a => flatMap(rb)( b => unit(f(a,b)) ) )
    }
    
    def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)((_,_)) // (a,b) => (a,b) )
    
    val randIntDouble: Rand[(Int, Double)] = both(int, double)
    val randDoubleInt: Rand[(Double, Int)] = both(double, int)
    
    // exercise 6.7 implent sequence for combining a lit of transitions into a single transition.
    // val rng: RNG = SimpleRNG(10)
    // val seqf = RNG.sequence(List(RNG.int,RNG.int,RNG.int))
    // seqf(rng)
    def sequence[A](rands: List[Rand[A]]): Rand[List[A]] = {
        rng =>
            rands.foldRight( (List[A](), rng) )({ 
                case (rand, (as, rngA)) =>
                        val (a, nextRng) = rand(rngA)
                        ( (a :: as), nextRng )
            })
    }
    
    // oops use map2 much cleaner
    def sequenceMap2[A](fs: List[Rand[A]]): Rand[List[A]] = 
         fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _)) // ra, rb and _::_ function for combining the values.
   
    def intBetween(fromInclusive: Int, toExclusive: Int): Rand[Int] = {
        rng =>
         val (n, s) = nonNegativeInt(rng)
         val inRange = if (n >= fromInclusive && n < toExclusive) n else n % toExclusive 
         (inRange, s)
     }
    
    def boolean: Rand[Boolean] = {
        rng =>
            val (n, nextRNG) = nonNegativeInt(rng)
            val b = if ( (n % 2) == 0 ) false else true 
            (b, nextRNG)
    }
// }
// 
// object Fun {
//     
    // example using Rand with nonNegative int
    def nonNegativeEven: RNG.Rand[Int] = RNG.map(nonNegativeInt)(i => i - i % 2)
    
    // exercise 6.6. use map to reimplemnt double in a more elegatnt way.
    def doubleMap: RNG.Rand[Double] = RNG.map(nonNegativeInt)(i => (Int.MaxValue / i.toDouble))
    
    // exercise 6.1
    // write a function that uses RNG.nextInt to generate a random int between 0 and Int.maxValue
    def nonNegativeInt(rng: RNG): (Int, RNG) = {
        val (n, nextRNG) = rng.nextInt
        val pos = if (n < 0) -(n + 1) else n
        (pos, nextRNG)
    }
    
    // exercise 6.2 Fun.double(SimpleRNG(10))
    // write a function to generate a Double between 0 and 1, not including 1.]
    def double(rng: RNG): (Double, RNG) = {
        val (numer, rng1) = nonNegativeInt(rng)
        val (divisor, rng2) = nonNegativeInt(rng1)
        val ranDouble: Double = (numer/divisor.toDouble)
        // if the random double is greater than one remove the integer part.
        val n: Double = if(ranDouble > 1) ranDouble.toInt/ranDouble
                        else ranDouble
        (n, rng2)
    }
    
    // exercise 6.3 write functions to generate an ((Int,Double), RNG))
    def intDouble(rng: RNG): ((Int,Double), RNG) = {
        val (nint,rng1) = rng.nextInt
        val (ndouble,rng2) = double(rng1)
        ((nint,ndouble), rng1)
    }
    
    def doubleInt(rng: RNG): ((Double,Int), RNG) = {
        val (ndouble,rng1) = double(rng)
        val (nint,rng2) = rng1.nextInt
        ((ndouble,nint), rng1)
    }
    
    def double3(rng: RNG): ((Double,Double,Double), RNG) = {
        val (d1,rng1) = double(rng)
        val (d2,rng2) = double(rng1)
        val (d3,rng3) = double(rng2)
        ((d1,d2,d3), rng3)
    }
    
    // ex 6.4 generate a list of random integer  Fun.ints(10)(SimpleRNG(10))
    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
        def go(n: Int, rng: RNG): List[Int] = {
            if (n <= 0) Nil
            else {
                val (n, nextRng) = rng.nextInt
                n :: go(n-1, nextRng)
            }
        }
        (go(count, rng), rng.nextInt._2)
    }
    
}