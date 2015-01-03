/**
 * Create paralle computation. can run on multi cores
 *
 **/
//sealed trait Par
case class Par1[Seq[_]] {
    
    def unit[A](as: Seq[A]): Par[Seq[A]]
    
    def map[A,B](seq: Seq[A])(f: A => B): Seq[B] = {
        // par should know n
        val numOfCores = 4
        val chunks = seq.groupBy(seq.size/numOfCores)
        chunks.map(futire { c => c.map(f) } ) // oncomplete
        def agregate(s)
        future{}
    }
}

type Par[A] = ExeutorService => Future[A]

class ExecutorService {
    def submit[A](callable: Callable[A]): Future[A]
}

trait Callable[A] { def call: A }

trait Future[A] {
    def get: A
    //def get(timeout: Long, unit: TimeUnit): A
    def cancel(evenIfRunning: Boolean): Boolean
    def isDone: Boolean
    def isCancelled: Boolean
}

object Par {
    
    def unit[A](a: A): Par[A] = { (es: ExecutorService) => UnitFuture(a) }
    
    def lazyUnit[A](a: => A): Par[A] = { fork(unit(a)) }    
    
    case class UnitFuture[A](get: A) extends Future[A] {
        def get: A = get
        //def get(timeout: Long, unit: TimeUnit): A
        def cancel(evenIfRunning: Boolean): Boolean = false
        def isDone: Boolean = true
        def isCancelled: Boolean = false
    }
    
    // TODO: fix to respect the timeout on Future.
    def map2[A,B,C](parA: Par[A], parB: Par[B])(f: (A,B) => C): Par[C] = {
        es =>
            val fa = parA(es)
            val fb = parB(es)
            UnitFuture(f(fa.get, fb.get))
    }
    
    // def map3[A,B,C,D](parA: Par[A], parB: Par[B], parC: Par[C])(f: (A,B,C) => D): Par[D] = {
    //     map2(parA, parB)()
    // }
    
    def map[A,B](parA: Par[A])(f: A => B): Par[B] = {
        map2(parA, unit(()))( (a,_) => f(a))
    }
    
    /**
     * Parallel map where a function is applied to every element 
     * in a given list in parallel.
     * run(es)(parMap(List(1,2,3,4))(_ + 3) // will not spawn the async computations until run is called.
     *
     * note: wrapped in fork "parMap will return immediately, even for a huge input list. 
     * When run is called, it will fork a single asynchronous computation which itslef spawns N parallel computations.
     *
     * @param ps the list contain the elements to be mapped over
     * @param f the function to apply to every element in a list
     * @return a parallel list of items in the original list transformed by the passed in function.
     */
    def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
                                // A => Par[B] asyncF(f)(_) don't need the (_) as it can be infered.
        val fbs: List[Par[B]] = ps.map( asyncF(f) ) // fork(lazyUnit(f(_))) )
        sequence(fbs)
    }
    
    // exercise 7.5
    def sequence[A](pas: List[Par[A]]): Par[List[A]] = {
        // unit(pas.foldRight(List[A]())( (a, as) => a.get :: as))
        pas.foldRight[ Par[List[A]] ](unit(List()))( (parA, parAs) => map2(parA, parAs)( (a, as) => a :: as ) )
    }
    
    // exercise 7.6
    /**
     * List[A] => Par[List[A]]
     *
     **/
    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
        //val fas: List[Par[A]]  = as.filter(asyncF(f))
        // via map
        val fasLs: List[Par[List[A]]]  = as.map(asyncF( a => if (f(a)) List(a) else Nil ))
        map(sequence(fasLs))(_.flatten)
    }
    
    // set up a Par to be run in a separate thread.
    def fork[A](a: => Par[A]): Par[A] = 
        es => es.sumbit( new Callable[A] { 
                            def call = a(es).get 
                         } 
                       ) 
    
    def run[A](es: ExecutorService)(a: Par[A]): Future[A] = a(es)
    
    // exercise 7.4 function to convert any function A => B to one that evaluates its results asynchronously 
    def asyncF[A,B](f: A => B): A => Par[B] = {
        a => fork(lazyUnit(f(a)))
    }
     
    def sortPar(parList: Par[List[Int]): Par[List[Int]] =
        //map2(parList,unit(()))( (a,_) => a.sorted )
        map(parList)(_.sorted)
    //es => fork(unit(parList.get.sort))

}

object examples {
    
    def sum1(xs: Seq[Int]): Int = xs.fold(0)(_ + _)
    
    def sum2(ints: IndexedSeq[Int]): Int {
        // if (ints.size == 0) 0
        //         else if (ints.size == 1) ints(0)
        if (ints.size <= 1) ints.headOption getOrElse 0
        else {
            val (right, left) = ints.sliptAt(ints.size/2)
            // add par parallel part
            val sumR = Par.unit(sum2(right)) // each half can be processed in parallel.
            val sumL = Par.unit(sum2(left))
            Par.get(sumR) + Par.get(sumL) // breaks RT get will wait for sumR to finish before moving on to sumL :(
        }
    }
    
    def sum3(ints: IndexSeq[Int]): Int {
        if (ints.size <= 1) ints.headOption getOrElse 0
        else {
            val (left, right) = ints.splitAt(ints.size/2)
            Par.map2(Par.unit(sum3(left)),  Par.unit(sum3(right)))(_ + _)
        }
    }
    
    def sum4(ints: IndexSeq[Int]): Int {
        if (ints.size <= 1) Par.unit(ints.headOption getOrElse 0)
        else {
            val (l, r) = ints.splitAt(ints.size/2)
            Par.map2(Par.fork(sum4(l)),  Par.fork(sum4(r)))(_ + _)
        }
    }
    
    def sum(ints: IndexSeq[Int]): Int {
        if (int.size <= 1) ints.headOption getOrElse 0
        else {
            val (l,r) = ints.splitAt(ints.size / 2)
            map2(fork(unit(sum(l))), fork(unit(sum(r)))(_ + _)
        }
    }
    
    def foldRightPar[A,B](as: IndexSeq[A])(z: B)(f: (A, B) => B): B {
        if (as.size <= 1) as.headOption getOrElse z
        else {
            val (l,r) = as.splitAt(ints.size / 2)
            map2(fork(unit(foldPar(l))), fork(unit(foldPar(r)))(f)
        }
    }
    
    def wordCount(paragraphs: List[String]): Int = {
        def countWords(para: String): Int = para.split(" ").size
        foldPar(paragraphs)(0)( (para,total) => countWords(para) + total )
    } 
}