object ListTest {

	import MyList._

	val a = List(1,2,3,4,5)
	val b = List(List(1),List(2),List(3),List(4),List(5))

	def test = {
		assert(map(a)(a => a+1) == List(2,3,4,5,6))
	}
}