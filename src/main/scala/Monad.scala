// First we need the Functor trait 
// A Functor can be defined as a data type that implements map.

trait Functor[F[_]] {
	def map[A,B](fa: F[A])(f: A => B): F[B]
}

// We parameterized map on the type constructor F[_].
// This means that we do not have to pick a particular F[_],
// but instead it's 'parametric' in the choice of F.

// Here's an instance for List of a Functor: 

val listFunctor = new Functor[List] { // <-- Parrametric choice of F right here
	def map[A,B](as: List[A])(f: A => B): List[B] = as.map(f) // as map f
}

// A Functor must abbide a law. 
// mapping over a structure x with the identity function 
// should itself be an identity. So mapping over something
// should preserve the structure of x, like this:
// map(x)(a => a) == x
// And here a => a is the identity function.

// Functors are a bit boring since it's not a whole lot that you can
// do with only the map function, we need more power!
// How about we make a trait, that also implements flatMap, and 
// extends Functor, so we get both map and flatMap?
// Let's do just that, and call it Monad. 

trait Monad[F[_]] extends Functor[F] {
	def unit[A](a: => A): F[A]
	def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]

	// We can implement map in terms of flatMap
	def map[A,B](ma: F[A])(f: A => B): F[B] = {
		flatMap(ma)(a => unit(f(a)))
	}
}

// We got a Monad trait, that if provided a unit and flatMap method
// now also provides the map method, since it's implementet in terms of 
// flatMap. The unit method simply takes a value, and wraps that value
// in the Monad context. Look at the example below to get the idea. 
// So how do we use it? Let's create a Monad for the List type.

object Monad {
  val listMonad = new Monad[List] {
  	def unit[A](a: => A): List[A] = List(a)
  	def flatMap[A,B](ma: List[A])(f: A => List[B]): List[B] = ma.flatMap(f)
  }
}

// Notice how we use the Lists built-in flatMap method to construct our
// List Monads flatMap method. 
// More functions can be defined in terms of unit, flatMap and map such as
// map2, sequence and traverse. But let's just let this sink in first.
// 
// Functors had a law, and Monads do as well.
// First of all, Monad extends Functor, so the laws that 
// was true for Functors, are also true for Monads.
// But Monads has laws of their own.
// The associative law states that monads should be associative.
// 
// x.flatMap(f).flatMap(g) == x.flatMap(a => f(a).flatMap(g))
// 
// So while combining Monad values into one, it should not matter
// if we do one or the other first. 
// 
// This can also be written as 
// 
// compose(compose(f, g), h) == compose(f, comopse(g,h))
// 
// Where compose means join the values together into one. 
// 
// Another law is the identity laws.
// This just means, that if you compose anything with a unit
// you should get that same thing back. 
// 
// compose(f, unit) == f
// compose(unit, f) == f
// 
// So that's actually two laws in one, since it should be true for both ways.
// Before we expressed the associativity law with flatMap and compose, and
// actually you can show the identity laws with flatMap as well.
// 
// flatMap(x)(unit) == x
// flatMap(unit(y))(f) == f(y)
//
// So what is a Monad, if you have to take all that and put it into one sentence?
// 
// "A Monad is an implementation of one of the minimal sets of monadic combinators, 
// satisfying the laws of associativity and identity." (Chiusano and Bjarnason, 2014, p. 199)
// 
// Where a "minimal set of monadic combinators" in our case is unit and flatMap.
// There are other minimal sets
// 
// * unit and compose
// * unit, map and compose
// 
// But let's not worry about that, since we are doing just fine with unit and flatMap.
// 
// REFERENCES 
// Chiusano, P., & Bjarnason, R. (2014). Functional Programming in Scala. Manning Publications Co.. ISO 690	
