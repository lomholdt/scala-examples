import scala.annotation.tailrec

/**
 * Object with typical List methods 
 * in different variations.
 * 
 * @author 		Jonas Lomholdt
 * @date        2016-01-17
 * @timestamp 	2016-01-17T21:14:13+0100
 */
object MyList {
	
	// map using pattern matching and recursion 
	def map[A,B](l: List[A])(f: A => B): List[B] = l match {
		case Nil => Nil
		case h::t => f(h) :: map(t)(f)
	}

	// map using pattern matching and wildcard basecase
	def map2[A,B](l: List[A])(f: A => B): List[B] = l match {
		case h::t => f(h) :: map2(t)(f)
		case _ => Nil
	}
	
	// flatMap using build-in flatten method
	// no pattern matching
	def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = {
		map(l)(f).flatten
	}	

	// recursive flatMap without use of builtin methods
	// using pattern matching
	def flatMap2[A,B](l: List[A])(f: A => List[B]): List[B] = l match {
		case Nil => Nil
		case h::t => f(h) ++ flatMap2(t)(f)
	}

	// 
	def flatMap3[A,B](l: List[A])(f: A => List[B]): List[B] = {
		l.foldLeft(List[B]())(_ ++ f(_))
	}

	def flatMap4[A,B](l: List[A])(f: A => List[B]): List[B] = {
		@tailrec
		def go(l: List[A], r: List[B])(f: A => List[B]): List[B] = l match {
			case Nil => r
			case h::t => go(t, r ++ f(h))(f)
		}
		go(l, List[B]())(f)
	}
}


