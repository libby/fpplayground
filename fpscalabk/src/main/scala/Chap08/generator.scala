import fpscala._

case class State [S,A](run: S => (A,S)) 

object State {
    
    def unit[S,A](a: A): State[S,A] = State(s => (a,s))

    def flatMap[S,A,B](sa: State[S,A])( f: A => State[S,B] ): State[S,B] = State({
        s => 
            val (a, nextS) = sa.run(s)
            f(a).run(nextS)
    })
    
    def sequence[S,A](ls: List[State[S,A]]): State[S, List[A]] = State[S,List[A]] (
        { s => 
            ls.foldRight( (List[A](), s) ) ( { 
                case (state, (as,prevState)) => 
                    val (a, nextState) = state.run(prevState)
                    (a::as, nextState)
                    
                })
        }
    )
    
    // def sequence[S,A](ls: List[State[S,A]]): State[List[A]] = 
    //         ls.foldRight(unit[S,List[A]](List[A]()))( 
    //             (s, acc) =>  
    //                 s.map2(acc)(_::_)
    //             )
    
}

// type Gen = State[Rand,Int]
// case class Gen[A] extends State[A, Rand]
case class Gen[A](sample: State[fpscala.RNG, A]) {
    
    // exercise 8.6
    def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(State.flatMap(sample)( a => f(a).sample ) )   
    
    // now that we have flatMap we can use it to generate an Int
    def listOfNFM(size: Gen[Int]): Gen[List[A]] = size.flatMap (n => Gener.listOfN(n, this)) 

}

import Prop._

sealed trait Result {
    def isFalsified: Boolean
}

case object Passed extends Result {
    def isFalsified = false
}

case class Falisified(failure: FailedCase, successes: SuccessCount) extends Result {
    def isFalsified = true
}

case class Prop(run: TestCases => Result)
// how many tests successed,
// what arguments failed the test.
object Prop {
    
    type FailedCase = Int
    type SuccessCount = Int
    type ErrorMessage = String
    type TestCases = Int
    // don't need Either as if pass, all pass
   // type Result = Option[FailedCase, SuccessCount)] //Either[(FailedCase, SuccessCount), SuccessCount]
    // val totalRan: Int
    // non-strict Either
    // need to specify how many test cases to examine before it passes.
    //def check: Either[(ErrorMessage,SuccessCount), SuccessCount]
    
}
 
// failed

object Gener {
    
    def unit[A](a: A): Gen[A] = Gen(State.unit(a))  // Gen(State({ rng => (a,rng) }))
    
    // 8.7
    def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = //g1.flatMap( a1 => g2.flatMap (a2 => Gener.boolean.flatMap ( p => Gener.unit(if (p) a1 else a2 )) ))
        Gener.boolean.flatMap ( p => if (p) g1 else g2)
    
    // 8.8 weighted Union
    def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double) ): Gen[A] = {
        val (w1,w2) = (g1._2, g2._2)
        Gener.choose(1,100).flatMap ( n => if ( (100 * w1) < n) g1._1 else g2._1 )
    }
    
    def weighted_bad[A](g1: (Gen[A],Double), g2: (Gen[A],Double) ): Gen[A] = {
        val (w1,w2) = (g1._2, g2._2)
        g1._1.flatMap( a1 => g2._1.flatMap ( a2 => Gener.choose(1,100).flatMap ( n => 
            Gener.unit(if ( (n * 100) < w1) a1 else a2 )) )
            )
    }
    
    // def flatMap[A,B](g: Gen[A])(f: A => Gen[B]): Gen[B] = Gen(State.flatMap(g.sample)( a => f(a).sample ) )
    
    def boolean: Gen[Boolean] = Gen(State(fpscala.RNG.boolean))
    
    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))
    // {
    //         
    //         def go(n: Int, gen: Gen[A], rng: RNG): (Gen[A], List[A]) = {
    //             if (n < 0) (gen, Nil)
    //             else {
    //                 val (a, g) = gen.sample.run(rng) // rng => ()
    //                 val (as, nextGen) = go(n-1, g)._2
    //                 (nextGen, a :: as)
    //             }
    //         }
    //         
    //         val (gen, as) = go(n, g, Fun.nonNegativeEven)
    //         Gen(s => (gen, as))
    //     }
    
    def choose(inclusive: Int, toExclusive: Int): Gen[Int] = Gen(State(fpscala.RNG.intBetween(inclusive, toExclusive)))
    
    // Gener.choose2(1,100).sample.run(SimpleRNG(10))
    def choose2(inclusive: Int, toExclusive: Int): Gen[(Int, Int)] = {
        val rng1 = RNG.intBetween(inclusive, toExclusive)
        val rng2 = RNG.intBetween(inclusive, toExclusive)
        // Gen( State( { 
        //                      RNG.flatMap(rng1)( n1 => RNG.flatMap(rng2)( n2 => RNG.unit((n1, n2))) ) 
        //                     }))
        Gen( State(RNG.map2(rng1, rng2)( (_,_) ) ))
    }
    
    //def toOption[A](maybeA: Gen[Option[A]]): Option[Gen[A]] = State.flatMap(maybeA.sample)( optA => optA.map(a => Option(Gener.unit(a))).getOrElse(None) ).sample
    //def toOption[A](genMaybeA: Gen[Option[A]]): Option[Gen[A]] = Option(Gen(Gener.flatMap(genMaybeA)( optA => optA.map(a => Option(Gener.unit(a))).getOrElse(None) ).sample))
    // def toOption[A](genMaybeA: Gen[Option[A]]): Option[Gen[A]] = {
    //         val f: Option[A] => Gen[Option[Gen[A]]] = optA => optA.map( a => Gener.unit(Some(Gener.unit(a))) ).getOrElse(Gener.unit(None))
    //         val s: State[fpscala.RNG, Option[Gen[A]]] = genMaybeA.flatMap(f).sample
    //         s
    //     }
    
}