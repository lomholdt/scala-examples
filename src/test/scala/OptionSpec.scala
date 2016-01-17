import org.scalatest.FlatSpec

class OptionSpec extends FlatSpec {

	import MyOption._

	val a: Option[Int] = Some(1)
	val b = None

	it should "(map) increment element by one" in {
		assert(map(a)(a => a+1) == Some(2))
	}

	it should "(flatMap) increment element by one" in {
		assert(flatMap(a)(a => Some(a+1)) == Some(2))
	}
	
	it should "(flatMap) return the same element" in {
		assert(flatMap(a)(a => Some(a)) == a)
	}

	it should "(flatMap) return None" in {
		assert(flatMap(b)(a => Some(a)) == b)
	}

	it should "(flatMap2) increment element by one" in {
		assert(flatMap2(a)(a => Some(a+1)) == Some(2))
	}
	
	it should "(flatMap2) return the same element" in {
		assert(flatMap2(a)(a => Some(a)) == a)
	}

	it should "(flatMap2) return None" in {
		assert(flatMap2(b)(a => Some(a)) == b)
	} 

}