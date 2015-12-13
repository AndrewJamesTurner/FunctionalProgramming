trait MyOption[+A] {
	
	def map[B](f: A => B) : MyOption[B] = this match {
		case Some(a) => Some(f(a))
		case None => None
	}

	def flatMap[B](f: A => MyOption[B]): MyOption[B] = {
		map(f).getOrElse(None)
	}
	
	def getOrElse[B >: A](default: => B): B = this match {
		case Some(a) => a
	}
	

	def orElse[B >: A](ob: => MyOption[B]): MyOption[B] = {
		Some(this).getOrElse(ob)
	}
	
	def filter(f: A => Boolean): MyOption[A] = {
		if ( map(f).getOrElse(false) ) this
		else None
	}

	def mean(s: Seq[Double]): MyOption[Double] = {
		if(s.isEmpty) None
		else Some( s.sum / s.length )
	}

	def variance(xs: Seq[Double]): MyOption[Double] = {
    	mean(xs).flatMap( m => mean(xs.map(x => math.pow(x - m, 2))))
	}
}

case class Some[+A](get: A) extends MyOption[A]
case object None extends MyOption[Nothing]