package example

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import Lists.max
import Lists.sum
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ListsSuite extends FunSuite {

  /**
   * Now we finally write some tests for the list functions that have to be
   * implemented for this assignment. We fist import all members of the
   * `List` object.
   */
  import Lists._

  /**
   * We only provide two very basic tests for you. Write more tests to make
   * sure your `sum` and `max` methods work as expected.
   *
   * In particular, write tests for corner cases: negative numbers, zeros,
   * empty lists, lists with repeated elements, etc.
   *
   * It is allowed to have multiple `assert` statements inside one test,
   * however it is recommended to write an individual `test` statement for
   * every tested aspect of a method.
   */
  test("sum of a few numbers") {
    assert(sum(List(1, 2, 0, -4)) === -1)
  }

  test("sum of empty list") {
    assert(sum(List()) === 0)
  }

  test("max of a few numbers") {
    assert(max(List(3, 7, 2)) === 7)
  }

  test("max of empty list") {
    intercept[NoSuchElementException] {
      max(List())
    }
  }
}
