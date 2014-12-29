package datastructures

import scala.annotation.tailrec

/**
 * :paste -raw  // if you want to support pacakge
 * import datastructures._
 * >:load ../datastructures/singlyLinkedList.scala
 */
sealed trait List[+A] {
    
    // cons
    def ::[A] (a: A) = new Cons(a, this)
    
}

case class Cons[A](head: A, tail: List[A]) extends List[A]

case object Nil extends List[Nothing] 

object List {
 
   // factor out sum and product
   def foldRight[A,B](l: List[A])(zero: B)(f: (A, B) => B): B =
       l match {
           case Nil => zero
           case Cons(h, t) => f(h, foldRight(t)(zero)(f)) 
       }
   
   def foldLeft[A,B](l: List[A], z: B)(f: (B,A) => B): B = {
       @tailrec
       def go(as: List[A], acc: B): B = as match {
           case Nil => acc
           case Cons(h, t) => go(t, f(acc,h))
       }
       go(l, z)
   }    
   
   def foldRightTail[A,B](l: List[A])(zero: B)(f: (A, B) => B): B = {
      
       @tailrec
       def go(as: List[A], acc: B): B = {
           as match {
                 case Nil => acc
                 case Cons(h, t) => go(t, f(h,acc))
            }
       }
       go(l, zero)
   }
   
   // ex 3.12 reverse List
   def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])( (bs, a) => Cons(a, bs) )
   
   // ex. 3.11 write sum, product and length using foldLeft
   def sumL(ints: List[Int]): Int = foldLeft(ints, 0)( _ + _ )
   def productL(ints: List[Double]): Double = foldLeft(ints, 1.0)( _ * _ )
   def lengthL[A](l: List[A]): Int = foldLeft(l, 0)( (b,a) => b + 1)
   
   def length[A](l: List[A]): Int = foldRight(l)(0)( (a,b) => b + 1)
   
   def sum2(ints: List[Int]): Int = foldRight(ints)(0)( (x,y) => x + y )
   
   def product2(ds: List[Double]): Double = foldRight(ds)(1.0)( _ * _)
   
   def sum(ints: List[Int]): Int = ints match {
       case Cons(head, t) => head + sum(t)
       case Nil => 0
   }
   
   /**
    * possible short circuit `case Cons(0.0, _) => 0.0`
    */
   def product(ds:  List[Double]): Double = ds match {
       case Cons(head, t) => head * product(t)
       case Nil => 1.0
   }
    
   def fill[A](n: Int, a: A): List[A] = {
       if(n <= 0) Nil
       else Cons(a, fill(n-1, a))
   }
   
   /**
    * ex 3.2 // throw new Exception("No such element")
    * Removes the first elemtn of a List. Constant time function
    * @param xs the List retrieve the tail from
    * @return the tail of the List. 
    */
   def tail[A](xs: List[A]): List[A] = xs match {
       case Nil => Nil 
       case Cons(h, t) => t
   }
   
   def drop[A](l: List[A], n: Int): List[A] = (l, n) match {
       case (Nil, _) => Nil
       case (_ , 0) => l
       case (Cons(h, t), n) => drop(tail(l), n - 1)
   }
   
   // def dropWhile[A](l: List[A], p: A => Boolean): List[A] = l match {
   //      case Nil => Nil
   //      case Cons(h, t) if p(h) => dropWhile(tail(l), p)
   //      case _ => l
   //  }

   /**
    * List.dropWhile(List(1,2,3,4))(x => x < 4)
    */
   def dropWhile[A](l: List[A])(p: A => Boolean): List[A] = l match {
      case Cons(h, t) if p(h) => dropWhile(tail(l))(p)
      case _ => l
   }
      
   // add all the elements of one list to the end of another
   def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
       case Nil => a2
       case Cons(h, t) => Cons(h, append(t, a2))
   }

   /**
    * returns a List consisting of all but the last element of a List
    * i.e. given List(1,2,3,4) return List(1,2,3)
    * because of the structure of a singly linked list whenever you want to replace the tail, 
    * all the he previous COns object must be copied.
    */
   def init[A](l: List[A]): List[A] = l match {
       case Nil => Nil
       case Cons(h, Nil) => Nil
       case Cons(h, t) => Cons(h, init(t))
   }
   
   /**
    * ex 3.3
    * replace the first elemtn of a `List` with a different value.
    * @param xs the List to replace the head element
    * @param x the new head element
    * @return the list xs with the original head element replaced with the element x.
    */
   def setHead[A](xs: List[A], x: A): List[A] = xs match {
       case Nil => List(x)
       case Cons(h, t) => Cons(x,t)
   }
   
   def apply[A](as: A*): List[A] = {
       if (as.isEmpty) Nil
       else Cons(as.head, apply(as.tail: _*))
   }
   
   val x = List(1,2,3,4,5) match {
       case Cons(x, Cons(2, Cons(4, _))) => x
       case Nil => 42
       case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // matches
       case Cons(h,t) => sum(t)
       case _ => 101
   } 
   
   // ex 3.16
   // Write a function that transforms a list of integers by adding 1 to  each element
   def addOne(as: List[Int]): List[Int] = as match {
       case Nil => Nil
       case Cons(h, t) => Cons(h + 1, addOne(t))
   }
   
   // ex 3.17
   // Write a function that turns each value in a List[Double] to String
   def double2String(ds: List[Double]): List[String] = ds match {
       case Nil => Nil
       case Cons(h, t) => Cons(h.toString, double2String(t))
   }

   // generalize
   def map[A,B](as: List[A])(f: A => B): List[B] = as match {
       case Nil => Nil
       case Cons(h, t) => Cons(f(h), map(t)(f))
   }
   
   def addOneM(xs: List[Int]): List[Int] = map(xs)(_ + 1)

   def double2StringM(ds: List[Double]): List[String] = map(ds)(_.toString)
   
   def forall[A](as: List[A])(p: A => Boolean): Boolean = as match {
       case Nil => true
       case Cons(h,t) if p(h) => forall(t)(p) 
       case _ => return false
   }
   
   // 3.19
   // write a function filter that removes elems unless they satisfy a predicate
   def filter[A](as: List[A])(p: A => Boolean): List[A] = as match {
       case Nil => Nil
       case Cons(h, t) if p(h) => Cons(h, filter(t)(p))
       case Cons(h, t) => filter(t)(p)
   }
   
   // 3.20 
   def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = as match {
       case Nil => Nil
       case Cons(h, t) => append( f(h), (flatMap(t)(f)) )
   }
   
   // 3.21 use flatMap to implment filter
   def flatMapFilter[A](as: List[A])(p: A => Boolean): List[A] = flatMap(as) {
       i =>
        if(p(i)) List(i)
        else Nil
   }
   
   def zip[A,B](xs: List[A], ys: List[B]): List[(A,B)] = (xs, ys) match {
       case (Cons(h,t), Cons(h2,t2)) => Cons((h -> h2), zip(t,t2))
       case (_, _) => Nil
   }
   
   // 3.22 Write a function that accepts two lists and constructs a new list by adding corresonding elemnts
   def sumTwoList(xs: List[Int], ys: List[Int]): List[Int] = flatMap(zip(xs, ys))(t => List(t._1 + t._2))
   
   def addTups(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
       case (Cons(h1, t1), Cons(h2,t2)) => Cons(h1 + h2, addTups(t1, t2))
       case _ => Nil
   }
   
   def zipWithFlat[A,B,C](as: List[A], bs: List[B])(f: ((A,B)) => List[C]): List[C] = flatMap(zip(as, bs))(f)
   
}
