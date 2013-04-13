package objsets

import common._
import TweetReader._

/**
 * A class to represent tweets.
 */
class Tweet(val user: String, val text: String, val retweets: Int) {

  override def toString: String =
    "User: " + user + " [" + retweets + "] " + " Text: " + text
}

/**
 * This represents a set of objects of type `Tweet` in the form of a binary search
 * tree. Every branch in the tree has two children (two `TweetSet`s). There is an
 * invariant which always holds: for every branch `b`, all elements in the left
 * subtree are smaller than the tweet at `b`. The eleemnts in the right subtree are
 * larger.
 *
 * Note that the above structure requires us to be able to compare two tweets (we
 * need to be able to say which of two tweets is larger, or if they are equal). In
 * this implementation, the equality / order of tweets is based on the tweet's text
 * (see `def incl`). Hence, a `TweetSet` could not contain two tweets with the same
 * text from different users.
 *
 * The advantage of representing sets as binary search trees is that the elements
 * of the set can be found quickly. If you want to learn more you can take a look
 * at the Wikipedia page [1], but this is not necessary in order to solve this
 * assignment.
 *
 * [1] http://en.wikipedia.org/wiki/Binary_search_tree
 */
abstract class TweetSet {

  /**
   * Returns a new `TweetSet` which contains all elements of this set, and the
   * the new element `tweet` in case it does not already exist in this set.
   *
   * If `this.contains(tweet)`, the current set is returned.
   */
  def incl(tweet: Tweet): TweetSet

  /**
   * Returns a new `TweetSet` which excludes `tweet`.
   */
  def remove(tweet: Tweet): TweetSet

  /**
   * Tests if `tweet` exists in this `TweetSet`.
   */
  def contains(tweet: Tweet): Boolean

  /**
   * This method takes a function and applies it to every element in the set.
   */
  def foreach(f: Tweet => Unit): Unit

  /**
   * Returns true if no tweets are contained
   */
  def isEmpty(): Boolean

  /**
   * This method takes a predicate and returns a subset of all the elements
   * in the original set for which the predicate is true.
   *
   */
  def filter(p: Tweet => Boolean): TweetSet = filterAcc(p, new Empty)

  /**
   * This is a helper method for `filter` that propagetes the accumulated tweets.
   */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  /**
   * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
   *
   */
  def union(that: TweetSet): TweetSet

  /**
   * Returns the tweet from this set which has the smallest retweet count.
   *
   * Calling `mostRetweeted` on an empty set should throw an exception of
   * type `java.util.NoSuchElementException`.
   *
   */
  def mostRetweeted: Tweet

  /**
   * Returns a list containing all tweets of this set, sorted by retweet count
   * in descending order. In other words, the head of the resulting list should
   * have the highest retweet count.
   *
   */
  def descendingByRetweet: TweetList = descendingByRetweetAcc(Nil)

  /**
   * Helper method to accumulate the TweetList for descendingByRetweet
   */
  def descendingByRetweetAcc(list: TweetList): TweetList

}

class Empty extends TweetSet {

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  def contains(tweet: Tweet): Boolean = false

  def foreach(f: Tweet => Unit): Unit = ()

  def isEmpty = true

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc

  def union(that: TweetSet): TweetSet = that

  def mostRetweeted = throw new NoSuchElementException

  def descendingByRetweetAcc(list: TweetList): TweetList = list
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def remove(tw: Tweet): TweetSet = {
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)
  }

  def contains(x: Tweet): Boolean = {
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true
  }

  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }

  def isEmpty = false

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
    val leftSet = left.filterAcc(p, acc)
    val rightSet = right.filterAcc(p, leftSet)
    if (p(elem)) {
      rightSet.incl(elem)
    } else {
      rightSet
    }
  }

  def union(that: TweetSet): TweetSet = right.union(left.union(that.incl(elem)))

  def mostRetweeted(): Tweet = {

    /**
     * Compare that TweetSet mostRetweeted tweet to otherTweet
     * The tweet with the least retweets is returned
     */
    def mostRetweetedHelper(that: TweetSet, otherTweet: Tweet): Tweet = {
      if (that.isEmpty) {
        otherTweet
      } else {
        val setTweet = that.mostRetweeted
        if (setTweet.retweets < otherTweet.retweets) {
          setTweet
        } else {
          otherTweet
        }
      }
    }
    mostRetweetedHelper(right, mostRetweetedHelper(left, elem))
  }

  def descendingByRetweetAcc(list: TweetList): TweetList = {
    val tweet = mostRetweeted
    val subSet = remove(tweet)
    subSet.descendingByRetweetAcc(new Cons(tweet, list))
  }

}

trait TweetList {
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
}

object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  def search(words: List[String], ts: TweetSet): TweetSet = searchInner(words, ts)

  /*
   * Search using explicit iterators. By returning, the search in a tweet can
   * be aborted once a match is found.
   */
  def searchInner(words: List[String], ts: TweetSet): TweetSet = {
    def searchTweet(tweet: Tweet): Boolean = {
      val textParts = tweet.text.split(" ")
      val textPartsIter = textParts.iterator
      while (textPartsIter.hasNext) {
        val textPart = textPartsIter.next
        val wordsIter = words.iterator
        while (wordsIter.hasNext) {
          val word = wordsIter.next
          if (word == textPart) {
            return true
          }
        }
      }
      false
    }
    ts.filter(searchTweet)
  }

  /*
   * *Incorrect* implementation of search using iterators. The result is
   * always false.
   */
  def searchAnon(words: List[String], ts: TweetSet): TweetSet = {
    ts.filter((tweet: Tweet) => {
      val textParts = tweet.text.split(" ")
      val textPartsIter = textParts.iterator
      while (textPartsIter.hasNext) {
        val textPart = textPartsIter.next
        val wordsIter = words.iterator
        while (wordsIter.hasNext) {
          val word = wordsIter.next
          if (word == textPart) {
            true
          }
        }
      }
      false
    })
  }

  /*
   * search implementation using foldLeft.
   * This implementation is not optimal since it does not stop searching 
   * even if a match has been found
   */
  def searchFold(words: List[String], ts: TweetSet): TweetSet = {
    ts.filter((tweet: Tweet) => {
      val textParts = tweet.text.split(" ")
      textParts.foldLeft(false)((found, textPart) => {
        found || words.foldLeft(false)((innerFound, word) => {
          innerFound || word == textPart
        })
      })
    })
  }

  lazy val googleTweets: TweetSet = search(google, TweetReader.allTweets)

  lazy val appleTweets: TweetSet = search(apple, TweetReader.allTweets)

  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */
  lazy val trending: TweetList = {
    val keyWords = google ++ apple
    val ts = search(keyWords, TweetReader.allTweets)
    ts.descendingByRetweet
  }
}

object Main extends App {
  // Print the trending tweets
  GoogleVsApple.trending foreach println
}
