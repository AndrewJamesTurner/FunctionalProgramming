// A comment!

/* Another comment */
/** A documentation comment */
object MyModule {

	def abs(n: Int): Int =
		if (n < 0) -n
		else n

	def factorial(n: Int): Int = {

		@annotation.tailrec
		def go(n: Int, acc: Int): Int =
			if(n<=0) acc
			else go(n-1, n*acc)

		go(n, 1)
	}

	def fibonacci(n: Int): Int = {

		@annotation.tailrec
		def go(n: Int, max: Int, curr: Int, prev: Int ): Int = {

			if(n-1 == max) curr
			else if(n == 1) go(n+1, max, 1, 0 )
			else go(n+1, max, curr+prev, curr)
		}
					
		go(0, n, 0, 0)
	}

	private def formatResult(name: String, n: Int, f: Int=>Int) = {
		val msg = "The %s of %d is %d."
		msg.format(name, n, f(n))
	}

	private def formatAbs(x: Int) = {

		val msg = "The absolute value of %d is %d" 
		msg.format(x, abs(x))
	}

	private def formatFactorial(x: Int) = {

		val msg = "The factorial of %d is %d" 
		msg.format(x, factorial(x))
	}

	private def formatFibonacci(x: Int) = {

		val msg = "The fibonacci of %d is %d" 
		msg.format(x, fibonacci(x))
	}

	def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {

		@annotation.tailrec
		def go(n: Int): Boolean = {

			if (n >= as.length ) true
			else if( gt(as(n-1), as(n) )) false
			else go(n+1)
		}
					
		go(1)
	}
	
	def partial1[A,B,C](a: A, f: (A,B) => C): B => C = {
	  
	  def func(x: B): C =  f(a,x)  
	  return func
	}
	
	def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
	  
	  //a => b => f(a, b)
	  
	  def func(a: A) : (B => C) = f(a, _ )
	  return func
	}
	
	def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
	  
	  def func(a: A, b: B) : C = f(a)(b)
	  return func
	}
	
	def compose[A,B,C](f: B => C, g: A => B): A => C = {
	  
	  def func(a: A) : C = f(g(a))
	  return func
	  
	}

	def main(args: Array[String]): Unit = {
		println(formatResult("Abs", -42, abs))
		println(formatResult("Factorial", 5, factorial))
		println(formatResult("Fibonacci", 6, fibonacci))

		println(formatResult("increment", 7, (x: Int) => x + 1))
		println(formatResult("increment2", 7, (x) => x + 1))
		println(formatResult("increment3", 7, x => x + 1))
		println(formatResult("increment4", 7, _ + 1))
		println(formatResult("increment5", 7, x => { val r = x + 1; r }))

		var a = Array(1, 2, 3, 4, 0)
		var b = Array()

		if( isSorted(b, (x:Int, y:Int) => x>y) ) println("sorted")
		else println("not sorted")
			
	}
}    