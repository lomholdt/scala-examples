import org.scalatest.FlatSpec

class ListSpec extends FlatSpec {

	import MyList._

	val a = List(1,2,3,4,5)
	val a1 = List(2,3,4,5,6)

	val b = List(List(1),List(2),List(3),List(4),List(5))
	val b1 = List(1,2,3,4,5)

	
	/**
	 * map
	 */
	it should "(map) increment every element by one" in {
		assert(map(a)(a => a+1) == a1)
	}	
	it should "(map2) increment every element by one" in {
		assert(map2(a)(a => a+1) == a1)
	}
	it should "(map3) increment every element by one" in {
		assert(map3(a)(a => a+1) == a1)
	}

	/**
	 * flatMap
	 */
	it should "(flatMap) every element" in {
		assert(flatMap(b)(b => b) == b1)
	}
	it should "(flatMap2) every element" in {
		assert(flatMap2(b)(b => b) == b1)
	}	
	it should "(flatMap3) every element" in {
		assert(flatMap3(b)(b => b) == b1)
	}		
	it should "(flatMap4) every element" in {
		assert(flatMap4(b)(b => b) == b1)
	}	
	it should "(flatMap5) every element" in {
		assert(flatMap5(b)(b => b) == b1)
	}

	/**
	 * foldRight
	 */
	it should "(foldRight) return the lists sum" in {
		assert( foldRight(a,0)((a,b) => a+b) == 15)
	}
	it should "(foldRight2) return the lists sum" in {
		assert( foldRight2(a,0)((a,b) => a+b) == 15)
	}

	/**
	 * foldLeft
	 */
	it should "(foldLeft) return the lists sum" in {
		assert( foldLeft(a,0)((a,b) => a+b) == 15)
	}
	
}