package cats
package data

import algebra.Order
import scala.collection.immutable.SortedSet

class NonEmptySet[A] private[cats] (val toSet: SortedSet[A]) extends Function1[A, Boolean] {
  def size: Int = toSet.size
  def apply(a: A): Boolean = toSet(a)
  def contains(a: A): Boolean = toSet(a)
  def filter(f: A => Boolean): Option[NonEmptySet[A]] = NonEmptySet(toSet.filter(f))
  def +(a: A): NonEmptySet[A] = new NonEmptySet(toSet + a)
  def -(a: A): Option[NonEmptySet[A]] = NonEmptySet(toSet - a)
  def |(that: NonEmptySet[A]): NonEmptySet[A] = new NonEmptySet(toSet | that.toSet)
  def &(that: NonEmptySet[A]): Option[NonEmptySet[A]] = NonEmptySet(toSet & that.toSet)
  def --(s: NonEmptySet[A]): Option[NonEmptySet[A]] = NonEmptySet(toSet -- s.toSet)
  def ++(s: NonEmptySet[A]): NonEmptySet[A] = new NonEmptySet(toSet ++ s.toSet)
}

object NonEmptySet  extends NonEmptySetInstances{
  def apply[A](as: A*)(implicit order:Order[A]): Option[NonEmptySet[A]] = apply(SortedSet(as:_*)(Order.ordering))

  def apply[A](as: SortedSet[A]): Option[NonEmptySet[A]] =
    if (as.isEmpty) None else Some(new NonEmptySet(as))

}

sealed abstract class NonEmptySetInstances {
   implicit def nonEmptySetInstance: Reducible[NonEmptySet] = new Reducible[NonEmptySet] {
      // Members declared in cats.Foldable
    def foldLeft[A, B](fa: NonEmptySet[A],b: B)(f: (B, A) => B): B = fa.toSet.foldLeft(b)(f)
    def foldRight[A, B](fa: NonEmptySet[A],lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa.toSet.foldRight(lb)(f)
    
    // Members declared in cats.Reducible
    def reduceLeftTo[A, B](fa: NonEmptySet[A])(f: A => B)(g: (B, A) => B): B = {
      val iter = fa.toSet.iterator
      val hd = iter.next() //Safe by construction
      iter.foldLeft(f(hd))(g)
    }
    def reduceRightTo[A, B](fa: NonEmptySet[A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] = {
     val iter = fa.toSet.iterator
      val hd = iter.next() //Safe by construction
      iter.foldRight(Eval.now(f(hd)))(g) 
    }

    //Overrides for perf
    override def isEmpty[A](fa: NonEmptySet[A]): Boolean = false
    override def nonEmpty[A](fa:NonEmptySet[A]): Boolean = true
   }
}
