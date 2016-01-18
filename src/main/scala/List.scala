import scala.annotation.tailrec

/**
 * Object with typical List methods 
 * in different variations.
 * 
 * @author	Jonas Lomholdt
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

	def unit[A] (a: => A): List[A] = List(a)

	// map using monadic style with unit method. 
	def map3[A,B](l: List[A])(f: A => B): List[B] = {
		flatMap(l)(a => unit(f(a)))
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

	// flatMap using Lists built-in method foldLeft
	def flatMap3[A,B](l: List[A])(f: A => List[B]): List[B] = {
		l.foldLeft(List[B]())(_ ++ f(_))
	}

	// flatMap that's tailrecursive a.k.a no stack overflow
	def flatMap4[A,B](l: List[A])(f: A => List[B]): List[B] = {
		@tailrec
		def go(l: List[A], r: List[B])(f: A => List[B]): List[B] = l match {
			case Nil => r
			case h::t => go(t, r ++ f(h))(f)
		}
		go(l, List[B]())(f)
	}

	// Use built-in flatMap method on List
	def flatMap5[A,B](l: List[A])(f: A => List[B]): List[B] = l.flatMap(f)

	// foldRight using pattern matching
	def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B = l match {
		case Nil => z
		case h::t => f(h, foldRight(t, z)(f))
	}

	// foldRight using .reverse to achieve tailrecursive function
	def foldRight2[A,B](l: List[A], z: B)(f: (A, B) => B): B = {
		foldLeft(l.reverse, z)((b, a) => f(a,b))
	}

	// tailrecursive version of foldLeft
	@tailrec
	def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
		case Nil => z
		case h::t => foldLeft(t, f(z,h))(f)
	}
}


