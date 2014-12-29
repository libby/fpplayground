sealed trait Either[+E, +A] {
    
    // ex 4.6
    def map[B](f: A => B): Either[E,B] = this match {
        case l@Left(e) => l
        case Right(a) => Right(f(a)) 
    }
    
    def flatMap[EE >: E, B](f: A => Either[EE,B]): Either[EE,B] = this match {
        case l@Left(e) => l
        case Right(a) => f(a)
    } // this map(f) orElse Left()
    
    def orElse[EE >: E, B >: A](b: => Either[EE,B]): Either[EE,B] = this match {
        case Left(_) => b
        case Right(_) => this
    }
    
    def map2[EE >: E, B, C](b: Either[EE,B])(op: (A,B) => C): Either[EE,C] = this.flatMap(a => b.map( b => op(a,b) ) )
        
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object play {
    
    // ex 4.7 return first error encountered
    def sequence[E,A](es: List[Either[E,A]]): Either[E, List[A]] = es match {
        case Nil => Right(Nil)
        case r:: t => r.flatMap(a => sequence(t).map( as => a :: as) ) 
    }
    
    def traverse[E,A,B](as: List[A])(f: A => Either[E,B]): Either[E,List[B]] = as match {
        case Nil => Right(Nil)
        case a :: t => f(a).flatMap( b => traverse(t)(f).map(bs => b :: bs))
    }
    
    def Try[A](a: => A): Either[Exception,A] = 
        try {
            Right(a)
        } catch {
            case e: Exception => Left(e)
        }

    def mean(xs: Seq[Int]): Either[String, Int] = 
        if (xs.size < 1) Left("Error: the Seq was empty, can't compute the mean")
        else Right( xs.sum/xs.size )
    
}