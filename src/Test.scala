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
  
}  
