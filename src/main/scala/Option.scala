/**
 * Object with typical Option methods 
 * in different variations.
 * 
 * @author    Jonas Lomholdt
 * @date      2016-01-17
 * @timestamp 2016-01-17T21:29:23+0100
 */
object MyOption {

	// map using pattern matching
	def map[A,B](o: Option[A])(f: A => B): Option[B] = o match {
		case None => None
		case Some(a) => Some(f(a))
	}


	def unit[A] (a: => A): Option[A] = Some(a)

	def map2[A,B](o: Option[A])(f: A => B): Option[B] = {
		flatMap(o)(a => unit(f(a)))
	}

	// flatMap using pattern matching
	def flatMap[A,B](o: Option[A])(f: A => Option[B]): Option[B] = o match {
		case None => None
		case Some(a) => f(a)
	}

	// flatMap without using pattern matching, instead using getOrElse
	// method defined on the Option type
	def flatMap2[A,B](o: Option[A])(f: A => Option[B]): Option[B] = {
		map(o)(f) getOrElse None // Equivalent to map(o)(f).getOrElse(None)
	}

}