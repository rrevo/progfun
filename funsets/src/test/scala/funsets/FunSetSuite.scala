package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }
  
  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   * 
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   * 
   *   val s1 = singletonSet(1)
   * 
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   * 
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   * 
   */
  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val set1To99 = (x:Int) => x > 0 && x < 100
    val set0To99 = (x:Int) => x >= 0 && x < 100
    val set1To100 = (x:Int) => x > 0 && x <= 100
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   * 
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {
    
    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3". 
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
      assert(contains(set1To99, 1))
      assert(contains(set1To99, 99))
      assert(!contains(set1To99, 0))
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
    new TestSets {
      val s0To100 = union(set0To99, set1To100)
      assert(contains(s0To100, 0))
      assert(contains(s0To100, 100))
      assert(!contains(s0To100, 101))
    }
  }
  
  test("intersect") {
    new TestSets {
      val s = intersect(s1, s2)
      assert(!contains(s, 1))
      assert(!contains(s, 2))
      assert(!contains(s, 3))
    }
    new TestSets {
      val s0To100 = intersect(set0To99, set1To100)
      assert(!contains(s0To100, 0))
      assert(!contains(s0To100, 100))
      assert(contains(s0To100, 99))
    }
  }
  
  test("diff") {
    new TestSets {
      val s0 = diff(set0To99, set1To100)
      assert(contains(s0, 0))
      assert(!contains(s0, 100))
      assert(!contains(s0, 1))
    }
  }
  
  test("filter") {
    new TestSets {
      val s0To49 = filter(set0To99, (x: Int) => x < 50)
      assert(contains(s0To49, 0))
      assert(!contains(s0To49, 100))
    }
  }
  
  test("forall") {
    new TestSets {
      assert(forall(set1To99, (x:Int) => contains(set0To99, x)))
      assert(!forall(set1To99, (x:Int) => x == 1))
    }
  }
  
  test("exists") {
    new TestSets {
      assert(exists(set1To99, (x:Int) => x == 98))
      assert(!exists(set1To99, (x:Int) => x == 0))
    }
  }
  
  test("map") {
    new TestSets {
      assert(map(singletonSet(2), (x: Int) => x)(2))
      assert(map(singletonSet(3), (x: Int) => x*x)(9))
    }
  }
}
