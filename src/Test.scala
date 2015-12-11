import fpinscala.datastructures._

object Test extends App {
  val x = MyList(1,2,3,4,5)
  val y = MyList(6,7,8,9)
  
  println(x)
  println(MyList.tail(x))
  println(MyList.drop(x, 7))
  
  def test(x: Int) : Boolean = {
    if(x < 3) true
    else false
  }
  
  
  println(MyList.dropWhile(x)( test) )
  println(MyList.setHead(x, 17))
  
  println(MyList.append(x, y))
  
  println(MyList.length(x))
  
  //println(MyList.init(x))
  
  println(MyList.foldRight(MyList(1,2,3), Nil:MyList[Int])(Cons(_,_)))
  
  println(MyList.sum(x))
  
  println(x)
  println(MyList.reverse(x))
  
  println(MyList.stringIfy(x))
  
  println(MyList.filter(x)(a => if(a%2 == 1) false else true))
  
  println(MyList.flatMap(MyList(1,2,3))(i => MyList(i,i)))
  
  
  println(MyList.combine(MyList(3,2,1), MyList(1,2,3,17))((a,b) => a+b))
  
  
}  
