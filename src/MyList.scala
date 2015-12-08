package fpinscala.datastructures

sealed trait MyList[+A]

case object Nil extends MyList[Nothing]
case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {
  
  def sum(ints: MyList[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }
  
  def product(ds: MyList[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs) 
  }
  
  def apply[A](as: A*): MyList[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  
  def head[A](ints: MyList[A]): A = ints match {
   // case Nil => Nothing // don't know what to return
    case Cons(x,xs) => x
  } 
    
  def tail[A](ints: MyList[A]): MyList[A] = ints match {
    case Nil => ints
    case Cons(x,xs) => xs
  }  
  
  
  
  def drop[A](l: MyList[A], n: Int): MyList[A]= {
    
    def go(l: MyList[A], n: Int): MyList[A] = {
      if(n == 0) l
      else go( tail(l), n-1)
    }

    go(l, n)
  }
  
 
  def dropWhile[A](l: MyList[A])(f: A => Boolean): MyList[A] = {
    
    def go(ints: MyList[A]): MyList[A] = {
      
      if( f( head(ints) ) ) go( tail(ints))  
      else ints
    }

    go(l)
  }
  
  
  def setHead[A](l: MyList[A], h: A): MyList[A] = l match {
    case Nil => MyList(h)
    case Cons(x,xs) => Cons(h, xs)
  }  
    
      
  val example = Cons(1, Cons(2, Cons(3, Nil)))
  val example2 = MyList(1,2,3)
  val total = sum(example)  
}


