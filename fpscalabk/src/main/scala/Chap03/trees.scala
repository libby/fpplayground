import scala.annotation.tailrec

/**
 * Binary Tree ADT: Algebraic Data Type.
 * 
 */
sealed trait Tree[A]

case class Leaf[A](v: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
    
    // 3.25
    // write a function size that counts the number of ndoes (leaves and branches) in a tree.
    def size[A](tree: Tree[A]): Int = tree match {
        case Leaf(_) => 1
        // case Branch(left, l: Leaf[A])  => 1 + size(left)
        // case Branch(l: Leaf[A], right) => 1 + size(right)
        case Branch(left, right) => size(left) + size(right)
    }
    
    // 3.26
    // write a function maximum that retuns the maximum element in a Tree[Int]
    def maximum(tree: Tree[Int]): Int = {
        
        def go(t: Tree[Int], max: Int): Int = t match {
            case Leaf(v) => if (v > max) v else max
            case Branch(left, right) => go(left, go(right, max))
        }
        
        go(tree, 0)
    }
    
    // 3.27
    // write a function depth that returns the maximum path length from the root of a tree to any leaf.
    def depth[A](tree: Tree[A]): Int = tree match {
        case Leaf(_) => 1
        case Branch(left, right) => size(left) max size(right) 
    }
    
    // def depth[A](tree: Tree[A]): Int = {
    //         // depth: Int, maxdepth: Int
    //         def go(t: Tree[A]): Int = t match {
    //             case Leaf(_) => 1 //if (depth > maxdepth) depth + 1 else maxdepth
    //             case Branch(left, right) => size(left) max size(right) //go(left) max go(right)//go(left, depth + 1, go(right, depth + 1, maxdepth))
    //         }
    //         go(tree) // , 0, 0)
    //     }
    
    // 3.28
    def map[A,B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
        case Leaf(a) => Leaf(f(a))
        case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }
    
    // don't have a zero elem
    def fold[A,B](tree: Tree[A])(f: A => B)(op: (B,B) => B): B = {
        tree match {
            case Leaf(a) => f(a)
            case Branch(left, right) => op(fold(left)(f)(op),fold(right)(f)(op))
        }
        // def inner(t: Tree[A], acc: B): B = {
        //            case Leaf(a) => f(a, acc)
        //            case Branch(la: Leaf[A], right: Branch[A]) => f(la.v, inner(right, acc))
        //            case Branch(left: Branch[A], ra: Leaf[A]) => f(ra.v, inner(left, acc))
        //            case Branch(left, right) => op(inner(left, acc), inner(right, acc))
        //        }
        //        inner(tree, zero)
    }
    
}

object TreeTest {
    
    val t = buildTree
    
    def buildTree = {
        Branch(Branch(Leaf(1), Branch(Leaf(2),  Leaf(3))), Leaf(4))
    }
    
    def buildTree2 = {
        val b = Branch(Branch(Leaf(1), Branch(Leaf(2),Leaf(3))), Leaf(4))
        Branch(Leaf(20), b)
    }
    
}