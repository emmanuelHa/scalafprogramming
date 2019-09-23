package datastructures

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  /*The sum of the
    empty list is 0. */
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](elems: List[A]): List[A] = elems match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, t) => t
  }


  def setHead[A](elems: List[A], newHead: A): List[A] = elems match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, t) => Cons(newHead, t)
  }

  def drop[A](elems: List[A], n: Int): List[A] =
    if(n <= 0) elems
    else elems match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n-1)
    }


  def dropWhile[A](elems: List[A])(f: A => Boolean): List[A] = elems match {
    case Cons(h, t) if(f(h)) => dropWhile(t)(f)
    case _ => elems
  }


  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, acc) => acc + 1)

  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z,h))(f)
    }

  def sum3(ns: List[Int]) =
    foldLeft(ns, 0)((x,y) => x + y)

  def product3(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  def length3[A](as: List[A]): Int =
    foldLeft(as, 0)((acc, _) => acc + 1)

  // Incomprehensible
  def reverse[A](ns: List[A]): List[A] = foldLeft(ns, List[A]())((acc, h) => Cons(h, acc))

  def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(reverse(l), z)((b,a) => f(a,b))

  def foldRightViaFoldLeft_1[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(l, (b:B) => b)((g,a) => b => g(f(a,b)))(z)

  def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B =
    foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_,_))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil:List[A])(append)

  def add1(ns: List[Int]): List[Int] =
    foldRight(ns, Nil: List[Int])((h, t) => Cons(h+1, t))

  def doubleToString(ns: List[Double]): List[String] =
    foldRight(ns, Nil: List[String])((h,t) => Cons(h.toString, t))

  // Pas tail recursive
  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((h,t) => Cons(f(h), t))

  // Tail recursive
  def map_tail_recursive[A,B](l: List[A])(f: A => B): List[B] =
    foldRightViaFoldLeft(l, Nil:List[B])((h,t) => Cons(f(h),t))


  def map_2[A,B](l: List[A])(f: A => B): List[B] = {
    val buf = new collection.mutable.ListBuffer[B]
    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h,t) => buf += f(h); go(t)
    }
    go(l)
    List(buf.toList: _*) // converting from the standard Scala list to the list we've defined here
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil:List[A])((h,t) => if(f(h)) Cons(h,t) else t)

  def filter_fold_left[A](l: List[A])(f: A => Boolean): List[A] =
    foldRightViaFoldLeft(l, Nil:List[A])((h,t) => if (f(h)) Cons(h,t) else t)

  def filter_2[A](l: List[A])(f: A => Boolean): List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(h,t) => if (f(h)) buf += h; go(t)
    }
    go(l)
    List(buf.toList: _*) // converting from the standard Scala list to the list we've defined here
  }


  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  def filter_via_flatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)


  def add_two_list(l1: List[Int], l2: List[Int]): List[Int] =
    (l1, l2) match {
      case (Nil, _) => l2
      case (_, Nil) => l1
      case(Cons(h1, t1), Cons(h2, t2)) => Cons(h1+h2, add_two_list(t1, t2))
    }

  def zipWith[A,B,C](l1: List[A], l2: List[B])(f: (A,B) => C): List[C] =
    (l1, l2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case(Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1,h2), zipWith(t1, t2)(f))
    }

  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
    case (_,Nil) => true
    case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(h,t) => hasSubsequence(t, sub)
  }


  /*

  foldLeft(Cons(1, Cons(2,
                        Cons(3, Nil)
                   )
               ), List[A]())((acc, h) => Cons(h, acc))

   f(z,h) => f(List[A], 1) = Cons(1, List[A])

  foldLeft(Cons(2,
                Cons(3, Nil)
               ), (List[A], 1) => Cons(1, List[A])
           )((acc, h) => Cons(h, acc))
  foldLeft(Cons(3, Nil), (acc, h) => Cons(h, acc)
              )), List[A]())((acc, h) => Cons(h, acc))
  foldLeft(Nil, 6)((x,y) => x + y)
6
 */

  /*
  foldLeft(Cons(1, Cons(2, Cons(3, Nil))), 0)((x,y) => x + y)
  foldLeft(Cons(2, Cons(3, Nil)), 1)((x,y) => x + y)
  foldLeft(Cons(3, Nil), 3)((x,y) => x + y)
  foldLeft(Nil, 6)((x,y) => x + y)
  6
   */


}
