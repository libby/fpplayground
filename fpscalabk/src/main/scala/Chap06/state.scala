//type State[S,+A] = S => (A,S)

case class State[S,+A](run: S => (A,S)) {
    
    //def unit[A](a: A): State[S,A] = State(s => (a,s))
    
    def map[B](f: A => B): State[S,B] = {
        this.flatMap(a => State.unit(f(a)))
    }
    
    def map2[B,C](s2: State[S,B])(f: (A,B) => C): State[S,C] = {
        flatMap( a => s2.flatMap( b => State.unit(f(a,b))) )
    }
    
    def flatMap[B](f: A => State[S,B]): State[S,B] = State({
        s =>
            val (a, nextState) = run(s) // where does the state for run come from, NEEDED to wrap it in State()
            f(a).run(nextState) // need to weave the nextState through
    })
    
   
}

object State {
    
    def unit[S,A](a: A): State[S,A] = State(s => (a,s))
    
    // State(lsa) + a => State(a::a)
    def sequence[S,A](ls: List[State[S,A]]): State[S,List[A]] = 
        ls.foldRight(State.unit[S,List[A]](List[A]())) ( 
            (s, acc) =>  s.map2(acc)(_::_)
        )
           
    def sequence2[A,S](ls: List[State[S,A]]): State[S,List[A]] = State({
         startState => 
             ls.foldRight((List[A](), startState)){ 
                 case (stateA, (as, prevState)) => 
                     val (nextA, nextS) = stateA.run(prevState)
                     ((nextA :: as), nextS)
             }
     })

    // Note to self, keep getting confused about the intial value, think "where does the initial value come from" => unit
    // def sequence[A,S](ls: List[State[A]]): S => State[List[A]] = {
    //     startState => 
    //         State( ls.foldRight((startState, List[A]()))( 
    //             (sa, (prevState, as)) => 
    //                 val (a, s) = sa.run(prevState)
    //                 (a :: as)
    //         )
    // }
    
}