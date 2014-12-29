package datastructures2

sealed trait Option[+A] {
    
    def map[B](f: A => B): Option[B] = this match {
        case Some(a) => Some(f(a))
        case None => None
    }
    
    def flatMap[B](f: A => Option[B]): Option[B] = this match {
        case Some(a) => f(a)
        case None => None
    }
    
    def filter(p: A => Boolean): Option[A] = this.flatMap( { a => if (p(a)) Some(a) else None } )
    
    def getOrElse[B >: A](default: => B): B = this match {
        case Some(a) => a
        case None => default
    }
    
    def orElse[B >: A](opt: => Option[B]): Option[B] = this.flatMap(a => opt)
    
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
    
    // ex 4.3
    // Write a generic function nap2 that combines tow Option values using a binary function
    def map2[A,B,C](optA: Option[A], optB: Option[B])(f: (A,B) => C): Option[C] = {
        optA.flatMap( a => optB.map( b => f(a,b)) )
    }
    
    // ex 4.4 if the input list returns any None the result should be None
    def sequence[A](aOps: List[Option[A]]): Option[List[A]] = {
        if (aOps.forall(_ != None)) Some(aOps.map{ case maybea: Some[A] => maybea.get })
        else None
    }
    
    import scala.collection.immutable.List
    def sequence_1[A](aOps: List[Option[A]]): Option[List[A]] = aOps match {
        case Nil => Some(Nil)
        case h :: t => h.flatMap(a => sequence(t).map ( as => a :: as ))
    }
    
    // Option.traverse_1(List(1,2,3,4))( { x => if(x>1) Some(x) else None})
    def traverse[A,B](as: List[A])(f: A => Option[B]): Option[List[B]] = as match {
        case Nil => Some(Nil)
        case a :: t => f(a).flatMap( b => traverse(t)(f).map( bs => b :: bs))
    }
    
    def traverse_1[A,B](as: List[A])(f: A => Option[B]): Option[List[B]] = sequence_1(as.map(f))
    
    def Try[A](a: => A) = {
        try {
            Some(a)
        } catch {
            case e: Exception => None
        }
    }
    
}

//import datastructures._

object OptPlay {
    
    // mean is now a Total Function
    def mean(xs: Seq[Double]): Option[Double] = {
        if (!xs.isEmpty) Some(xs.sum / xs.size)
        else None
    }
    
    // variance(Seq(1.0,2.0,2.0,2.0,5.0))
    // math.pow(x - m, 2)
    def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap( m => mean(xs.map(x => math.pow(x - m, 2))) )
}

case class Employee(name: String, department: String)
object EmployeeScratch {
    import scala.collection.immutable.List
    val employees = List(Employee("joe","gaming"), Employee("joe","programming"))
    def lookupByName(name: String): Option[Employee] = {
        val maybeEmployees = employees.filter(_.name == name)
        if (maybeEmployees.size > 0) Some(maybeEmployees(0)) else None
    }
    lookupByName("idris").map(_.department)
}


