package example

import java.util.NoSuchElementException

import scala.annotation.tailrec

object Lists {
  /**
   * This method computes the sum of all elements in the list xs.
   *
   * @param xs A list of natural numbers
   * @return The sum of all elements in `xs`
   */
  def sum(xs: List[Int]): Int = {

    @tailrec
    def sum(acc: Int, xs: List[Int]): Int = {
      if (xs.isEmpty)
        acc
      else
        sum(acc + xs.head, xs.tail)
    }
    sum(0, xs);
  }

  /**
   * This method returns the largest element in a list of integers. If the
   * list `xs` is empty it throws a `java.util.NoSuchElementException`.
   *
   * @param xs A list of natural numbers
   * @return The largest element in `xs`
   * @throws java.util.NoSuchElementException if `xs` is an empty list
   */
  def max(xs: List[Int]): Int = {

    @tailrec
    def max(currentMax: Int, xs: List[Int]): Int = {
      if (xs.isEmpty)
        currentMax
      else {
        if (currentMax > xs.head)
          max(currentMax, xs.tail)
        else
          max(xs.head, xs.tail)
      }
    }

    if (xs.isEmpty)
      throw new NoSuchElementException
    else
      max(xs.head, xs.tail)
  }
}
