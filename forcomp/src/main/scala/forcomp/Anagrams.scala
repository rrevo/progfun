package forcomp

import common._

object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  type Occurrence = (Char, Int)

  /**
   * `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[Occurrence]

  /**
   * The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /**
   * Converts the word into its character occurence list. The list is sorted by the characters
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   */
  def wordOccurrences(w: Word): Occurrences = {
    val charToStringMap = w.toLowerCase().groupBy(ch => ch)
    val charToIntMap = charToStringMap.map(kv => (kv._1, kv._2.length()))
    val occurences = charToIntMap.toList.sortWith((a, b) => a._1 <= b._1)
    occurences
  }

  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = {
    val word = s.flatten(ch => ch)
    wordOccurrences(word.mkString)
  }

  /**
   * The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = {
    dictionary.groupBy(wordOccurrences(_))
  }

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = {
    val occ = wordOccurrences(word)
    val option = dictionaryByOccurrences.get(occ)
    option match {
      case Some(words) => words
      case None => Nil
    }
  }

  /**
   * Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  def combinations(occs: Occurrences): List[Occurrences] = {
    // Expand List(('a', 2), ('b', 1)) to Map(a -> List((a,1), (a,2)), b -> List((b,1))) 
    val occsAll = (for (occ <- occs; i <- 1 to occ._2) yield (occ._1, i)).groupBy(occ => occ._1)

    def merge(a: Occurrences, b: Occurrences): Occurrences = {
      (a ++ b).sortWith((x: Occurrence, y: Occurrence) => x._1 <= y._1)
    }

    def multiply(thisOccs: List[Occurrences], otherOccs: List[Occurrences]): List[Occurrences] = {
      val mergedOcc = for {
        occ <- thisOccs
        otherOcc <- otherOccs
      } yield {
        merge(occ, otherOcc)
      }
      // Return a list of all occs at this level + those
      // formed by merging with other occs
      thisOccs ++ mergedOcc
    }

    def combinations(occMap: Map[Char, Occurrences]): List[Occurrences] = {
      if (occMap.isEmpty) {
        Nil
      } else if (occMap.size == 1) {

        // occMap ~= Map(a -> List((a,1), (a,2)))
        val vals = occMap.head._2
        vals.map(occ => List(occ))

      } else {

        // Map(b -> List(List((b,1))), a -> List(List((a,1)), List((a,2))))
        val keyCombs = for (kv <- occMap) yield ((kv._1, combinations(Map(kv._1 -> occMap.get(kv._1).get))))

        // Map(b -> List(List((a,1)), List((a,2))), a -> List(List((b,1))))
        val otherOccs = for (kv <- occMap) yield ((kv._1, occMap - kv._1))
        val otherOccCombs = for (kv <- otherOccs) yield ((kv._1, combinations(kv._2)))

        val mutiplied = (for (kv <- keyCombs) yield (multiply(keyCombs.get(kv._1).get, otherOccCombs.get(kv._1).get))).flatten
        mutiplied.toSet.toList
      }
    }
    // We always have an empty list as one combination
    // Append with calculated combinations
    List(List()) ++ combinations(occsAll)
  }

  def occurrencesAsMap(y: Occurrences): Map[Char, Int] = {
    val yAsMap: Map[Char, Occurrences] = y.groupBy((occ: Occurrence) => occ._1)
    yAsMap.map(kv => (kv._1, yAsMap.get(kv._1).get.head._2))
  }

  /**
   * Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    val yAsFlatMap: Map[Char, Int] = occurrencesAsMap(y)
    val xReduced = for (occ <- x) yield {
      val yInt = yAsFlatMap.get(occ._1) match {
        case Some(i) => i
        case None => 0
      }
      (occ._1, occ._2 - yInt)
    }
    xReduced.filter(occ => occ._2 > 0)
  }

  /*
   * is other a subset of all
   */
  def occSubset(all: Occurrences, other: Occurrences): Boolean = {
    if (other.length > all.length) {
      false
    } else {
      val allMap = occurrencesAsMap(all)
      other.foldLeft(true)((isSubSet: Boolean, occ: Occurrence) => isSubSet &&
        (allMap.get(occ._1) match {
          case Some(i) => i >= occ._2
          case None => false
        }))
    }
  }

  def lengthOfOccurrence(occs: Occurrences): Int = {
    occs.foldLeft(0)((sum: Int, occ: Occurrence) => sum + occ._2)
  }

  def lengthOfSentence(sentence: Sentence): Int = {
    sentence.foldLeft(0)((sum: Int, word: Word) => sum + word.length())
  }

  def anagrams(occs: Occurrences): List[Sentence] = {
    if (occs.isEmpty) {
      List(Nil)
    } else {
      val len = lengthOfOccurrence(occs)

      // Get dictionary words that are a subset 
      val subsetDictOccs: Map[Occurrences, List[Word]] = for {
        kv <- dictionaryByOccurrences
        dictOcc = kv._1
        if occSubset(occs, dictOcc)
      } yield {
        dictOcc -> kv._2
      }

      // Get the anagrams for the remaining occs after the dictWord
      val dictAnagrams: Map[Occurrences, List[Sentence]] = (
        for {
          kv <- subsetDictOccs
          dictOcc = kv._1
          restOcc = subtract(occs, dictOcc)
        } yield {
          dictOcc -> anagrams(restOcc)
        }).toMap

      // Choose valid word/anagram combinations by checking length
      val filteredDictAnagrams: Iterable[(Occurrences, Sentence)] =
        for {
          kv <- dictAnagrams
          dictOcc = kv._1
          dictWords: List[Word] = subsetDictOccs.get(kv._1).get
          wordLen = dictWords.head.length()
          restAnagram <- kv._2
          if (lengthOfSentence(restAnagram) + wordLen) == len
        } yield {
          dictOcc -> restAnagram
        }

      // Get list of sentences that are valid by word*anagrams
      val sentences: Iterable[Sentence] = for {
        kv <- filteredDictAnagrams
        dictWords: List[Word] = subsetDictOccs.get(kv._1).get
        dictWord <- dictWords
        filteredDictAnagram = kv._2
      } yield dictWord :: filteredDictAnagram
      sentences.toList
    }
  }

  /**
   * Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    anagrams(sentenceOccurrences(sentence))
  }

}