object Par {
    
    // unit of parellel computation
    // primative combinator
    def unit[A](a: A): Par[A]
    
    // Derived cominator
    def lazyUnit[A](a: A): Par[A] = fork(unit(a))
    
    // get the value 
    // extracts a value from a `Par` by actually performing the computation.
    def run[A](par: Par[A]): A
    
    //def map2[A](a1: Par[A], a2: Par[A])(op: (A, A) => A): A
    
    // run in a separate thread
    // because of fork we can now make map2 strict and leave it up to 
    // marks a computation for concurrent ealuation.
    // eval won't happen till run
    def fork[A](a: => Par[A]): Par[A]
    
    // TODO: fix to respect the timeout on Future.
    def map2[A,B](parA: Par[A], parB: Par[B])(f: (A,B) => C): Par[C] = {
        es =>
            val fa = parA(es)
            val fb = parB(es)
            // loop to test for timeout?
            val starTime = System.millis
            def currentTime = System.millis
            val unitF = UnitFuture(f(fa.get, fb.get))
            def testForTimeout[A](f: Future[A]): Future[A] = {
                val runtime = currentTime - startTime
                if (f.timeout > runtime) {
                    f.cancel(true)
                    f
                } else testForTimeout(f)
            }
            testForTimeout(unitF)
            
    }
}
