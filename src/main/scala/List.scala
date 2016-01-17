

/**
 * Object with List methods in 
 * different variations
 * 
 * @author Jonas Lomholdt
 * @date 17/01-2016
 */
object MyList {
	
	/**
	 * map
	 */
	
	def map[A,B](l: List[A])(f: A => B): List[B] = l match {
		case Nil => Nil
		case h::t => f(h) :: map(t)(f)
	}

	def map2[A,B](l: List[A])(f: A => B): List[B] = l match {
		case h::t => f(h) :: map(t)(f)
		case _ => Nil
	}


	/**
	 * flatMap
	 */
	
	def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = {
		map(l)(f).flatten
	}	

	def flatMap2[A,B](l: List[A])(f: A => List[B]): List[B] = l match {
		case Nil => Nil
		case h::t => f(h) ++ flatMap(t)(f)
	}

	def flatMap3[A,B](l: List[A])(f: A => List[B]): List[B] = {
		l.foldLeft(List[B]())(_ ++ f(_))
		
	}

	def flapMap4[A,B](l: List[A])(f: A => List[B]): List[B] = {
		// Closure function
		def go(l: List[A], r: List[B])(f: A => List[B]): List[B] = l match {
			case Nil => Nil
			case h::t => go(t, r ++ f(h))(f)
		}
		go(l, List[B]())(f)
	}
}


