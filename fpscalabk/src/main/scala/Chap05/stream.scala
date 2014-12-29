package datastructures.fp

sealed trait Stream[+A] {
    
    // 5.1 write a function to convert a `Stream` to a `List`
    // force its evaluation and let you look at it in the REPL
    def toList: List[A] = this match {
        case Empty => Nil
        case Cons(h, tail) => h() :: tail().toList
    }
    
    // 5.2
    def take(n: Int): Stream[A] = this match {
        case Empty => Empty
        case Cons(h, tail) if n > 0 => Cons(h, () => tail().take(n-1))
        case _ => Empty
    }
    
    // 5.2
    def drop(n: Int): Stream[A] = this match {
        case Empty => Empty
        case Cons(h, tail) if n <= 0 => Cons(h, tail)
        case Cons(h, tail) if n > 0 => tail().drop(n-1)
    }
    
    // 5.3 return all starting elements of a Stream that match the given predicate.
    def takeWhile(p: A => Boolean): Stream[A] = this match {
        case Empty => Empty
        case Cons(h, tail) if p(h()) => Cons(h, () => tail().takeWhile(p))
        case _ => Empty
    }
    
    def exists(p: A => Boolean): Boolean = this match {
        case Empty => false
        case Cons(h, tail) => p(h()) || tail().exists(p)
    }
    
    def foldRight[B](z: => B)( f: (A, => B) => B): B = this match {
        case Empty => z
        case Cons(h, tail) => f(h(), tail().foldRight(z)(f))
    }
    
    def existsFR(p: A => Boolean): Boolean = foldRight(false)( (a, b) => p(a) || b )

    def forAllFR(p: A => Boolean): Boolean = foldRight(true)( (a, b) => p(a) && b )
    
    // exercise 5.4 implement forAll, which checks that all elements
    // in the `Stream` match a given predicate. Should terminate the traversal as soon as it encounters a nonmatching value
    def forAll(p: A => Boolean): Boolean = this match {
        case Empty => true
        case Cons(h, tail) => p(h()) && tail().forAll(p)
    }
   
    // 5.5. harder than 5.6
    def takeWhileFR(p: A => Boolean): Stream[A] = 
        foldRight(Stream.empty[A])( (a,b) => if (!p(a)) Empty else Cons(() => a, () => b) )
    
    // 5.6 imple using foldRight    
    def headOption: Option[A] = foldRight(None:Option[A])((a, optB) => Some(a))
    
    // 5.13 - use unfold implement map, take, takewhile, zipwith zipAll
    def map[B](f: A => B): Stream[B] = Stream.unfold(this) {
        case Empty => None
        case Cons(h, tail) => Some( (f(h()), tail()) )
    }
    
    def takeUnfold(n: Int): Stream[A] = Stream.unfold( (this,n) ) {
        case (Cons(h,tail), n) if n > 0 => Some(h(), (tail(), n-1)) 
        case _ => None
    }
    
    def takeWhileUnfold(p: A => Boolean): Stream[A] = Stream.unfold(this) {
        case Cons(h,tail) if p(h()) => Some(h(), tail()) 
        case _ => None
    }
    
    // 
    def zipwith[B](bs: Stream[B]): Stream[(A,B)]= Stream.unfold((this,bs)) {
        case (Cons(h1,t1), Cons(h2,t2)) => Some( (h1(),h2()), (t1(),t2()))
        case _ => None
    }
    
    def zipAll[B](bs: Stream[B]): Stream[(Option[A],Option[B])] = Stream.unfold((this,bs)) {
        case (Cons(h1,t1), Cons(h2,t2)) => Some( (Some(h1()), Some(h2())), (t1(),t2()))
        case (Cons(h1,t1), Empty) => Some( (Some(h1()),None), (t1(),Empty))
        case (Empty, Cons(h2,t2)) => Some( (None,Some(h2())), (Empty,t2()))
        case _ => None
    }
    
    // 5.14 startsWith - check whether one  Stream is prefix of another, i.e. Stream(1,2,3) startsWith Stream(1,2) == true
    // Stream(1,2,3,4,5).startsWith(Stream(1,2,3))
    def startsWith[A](s2: Stream[A]): Boolean = this.zipAll(s2).forAll { 
                                                    case (Some(h1), Some(h2)) =>  h1 == h2
                                                    case (Some(h1), None) => true
                                                    case (None, Some(h1)) => false
                                                    case (None, None) => true
                                                    }
    
    // TODO: tails and hasSubsequnce                                                
    
}

case object Empty extends Stream[Nothing]
case class Cons[+A](head: () => A,tail: () => Stream[A]) extends Stream[A]

object Stream {
    
    def cons[A](head: => A, tail: => Stream[A]): Stream[A] = {
        lazy val h = head
        lazy val t = tail
        Cons(() => h, () => t)
    }
    
    def empty[A]: Stream[A] = Empty
    
    def apply[A](elems: A*): Stream[A] = {
        if (elems.isEmpty) empty else cons(elems.head, apply(elems.tail: _*))
    }
    
    // 5.8
    def constant[A](a: A): Stream[A] = Cons(() => a, () => constant(a))
    
    // 5.9
    def from(n: Int): Stream[Int] = Cons(() => n, () => from(n + 1))
    
    // 0 1 1 2 3 5
    def fibs: Stream[Int] = { 
        def go(prev: Int, next: Int): Stream[Int] = Cons(() => prev, () => go(next, prev + next))
        go(0,1)
    }
    
    // 5.11 general unfold, takes and initial state, and a function for producting both the next
    // state and the next value in the generated Stream
    def unfold[A,S](z: S)(f: S => Option[(A,S)]): Stream[A] = {
        f(z) match {
            case Some( (a,s) ) => Cons(() => a, () => unfold(s)(f))
            case _ => Empty
        }
        
    }
    
    // 5.12 - implement fibs, from, constant, and ones using unfold
    def fibsUnfold: Stream[Int] = unfold( (0,1) )( { case (prev, next) => Some( prev, (next, prev + next) ) } )
    
    def fromUnfold(n: Int): Stream[Int] = unfold(n)( n => Some(n, n + 1) )
    
    def constantUnfold[A](a: A): Stream[A] = unfold(a)(a => Some(a,a) )
    
    def ones: Stream[Int] = unfold(1)( _ => Some(1,1))
    
}