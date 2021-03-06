import org.scalatest.BeforeAndAfter
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

// 0, 1, 2
// 2 -> {0, 1}
// 1 -> {0}
//
// Order: 2, 1, 0

class TestOrderSuite extends AnyFunSuite with BeforeAndAfter with Matchers {

  test("Graph should get parents for Node") {
    val dependencies = Map(
      "2" -> Seq("0", "1"),
      "1" -> Seq("0")
    )

    val graph = Graph(dependencies)

    graph.parents("0") should equal (Seq("2", "1"))
  }

  test("Method should order dependencies") {
    val dependencies = Map(
      "2" -> Seq("0", "1"),
      "1" -> Seq("0")
    )
    val expected = Seq("2", "1", "0")

    val result = SortUtils.sort(dependencies, parallel = true)

    result should equal (expected)
  }

  test("Method should avoid Race Condition") {
    val dependencies = Map(
      "0" -> Seq("1", "2"),
      "1" -> Seq("3"),
      "2" -> Seq("3"),
    )
    val expected_first = Seq("0", "2", "1", "3")
    val expected_second = Seq("0", "1", "2", "3")

    val result = SortUtils.sort(dependencies, parallel = true)

    result should (equal (expected_first) or equal (expected_second))
  }

  test("Method should produce RuntimeException in case of circular dependencies") {
    val dependencies = Map(
      "2" -> Seq("0", "1"),
      "1" -> Seq("0", "2")
    )
    assertThrows[RuntimeException] {
      SortUtils.sort(dependencies)
    }
  }
}
