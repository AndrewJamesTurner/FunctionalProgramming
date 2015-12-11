package fpinscala.datastructures

sealed trait MyList[+A]

case object Nil extends MyList[Nothing]
case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {
  
  def apply[A](as: A*): MyList[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  
  def foldRight[A,B](l: MyList[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }  
  
  
  def foldLeft[A,B](l: MyList[A], z: B)(f: (B, A) => B): B = {
    
   var acc = z
   var these = l
      
   while(  !isEmpty(these) ) {
     acc = f(acc, head(these))
     these = tail(these)
   }
  
   acc
  }
  
  
  def reverse[A](l: MyList[A]): MyList[A] = {
    foldLeft(l, MyList[A]())((a,b) => append(MyList[A](b), a))
  }
  
  def isEmpty[A](l: MyList[A]): Boolean = l match {
    case Nil => true
    case Cons(x,xs) => false
  }   
  
  def sum(i: MyList[Int]): Int = {
    foldLeft(i, 0)((a,b) => a + b)
  }
  
  def product(ds: MyList[Double]): Double =  {    
    foldRight(ds, 1.0)((a,b) => a * b)
  }
  
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
  
  /*
    def length[A](l: MyList[A]): Int = {
    
      def go(l: MyList[A], n: Int): Int = l match {
        case Nil => n
        case Cons(x,xs) => go(xs, n+1)
      }  

      go(l, 0)
  }
  */
  
  def length[A](l: MyList[A]): Int = {
    foldRight(l, 0)((a,b) => b+1)
  }
  
  
  def addOne(l: MyList[Int]): MyList[Int] = {
    foldLeft(l, MyList[Int]())((a,b) => append(a, MyList(b+1)))
  }
  
  def stringIfy(l: MyList[Int]): MyList[String] = {
    foldLeft(l, MyList[String]())((a,b) => append(a, MyList(b.toString)))
  }
  
  
  def map[A,B](l: MyList[A])(f: A => B): MyList[B] = {
    foldLeft(l, MyList[B]())((a,b) => append(a, MyList(f(b))))
  }
  
  def filter[A,B](l: MyList[A])(f: A => Boolean): MyList[A] = {
    //foldLeft(l, MyList[A]())((a,b) => if( f(b) ) append(a, MyList(b)) else a)
    flatMap(l)((a) => if(f(a)) MyList(a) else MyList())
  }
  
  def hasSubsequence[A](l: MyList[A], sub: MyList[A]): Boolean = {
    ???
  }
  
  def combine[A](l1: MyList[A], l2: MyList[A])(f: (A,A) => A ): MyList[A] = {
     
    var longList = if(length(l1) >= length(l2)) l1 else l2
    var shortList = if(length(l1) >= length(l2)) l2 else l1
    var result = MyList[A]()
    
    while(!isEmpty(shortList)) {    
      result = append(result, MyList[A](f(head(shortList), head(longList))))
      longList = tail(longList)
      shortList = tail(shortList)
    }
    
    while(!isEmpty(longList)) {    
      result = append(result, MyList[A](head(longList)))
      longList = tail(longList)
    }
    
     result
  }
  
  def flatMap[A,B](l: MyList[A])(f: A => MyList[B]): MyList[B] = {
    foldLeft(l, MyList[B]())((a,b) => append(a, f(b)) )
  }
  
  
  def append[A](a1: MyList[A], a2: MyList[A]): MyList[A] =
    a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
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
  
  /*
  def init[A](l: MyList[A]): MyList[A] = {
    
    def go(list: Cons[A]) : MyList[A] = list.tail match {
      case Nil => Cons(list, Nil)
      case Cons(x,xs) => Cons(go()
        
  
    }
    
    go(l)
    
  }
     */
  
  val example = Cons(1, Cons(2, Cons(3, Nil)))
  val example2 = MyList(1,2,3)
  val total = sum(example)  
}


