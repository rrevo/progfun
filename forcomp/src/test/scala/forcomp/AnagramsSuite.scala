package forcomp

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Anagrams._

@RunWith(classOf[JUnitRunner])
class AnagramsSuite extends FunSuite {

  test("wordOccurrences: abcd") {
    assert(wordOccurrences("abcd") === List(('a', 1), ('b', 1), ('c', 1), ('d', 1)))
  }

  test("wordOccurrences: Robert") {
    assert(wordOccurrences("Robert") === List(('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)))
  }

  test("sentenceOccurrences: abcd e") {
    assert(sentenceOccurrences(List("abcd", "e")) === List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)))
  }

  test("sentenceOccurrences: abcd Robert") {
    assert(sentenceOccurrences(List("abcd", "Robert")) ===
      List(('a', 1), ('b', 2), ('c', 1), ('d', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)))
  }

  test("dictionaryByOccurrences.get: eat") {
    assert(dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet)
      === Some(Set("ate", "eat", "tea")))
  }

  test("word anagrams: married") {
    assert(wordAnagrams("married").toSet === Set("married", "admirer"))
  }

  test("word anagrams: player") {
    assert(wordAnagrams("player").toSet === Set("parley", "pearly", "player", "replay"))
  }

  test("word anagrams: fakeword") {
    assert(wordAnagrams("fakeword").toSet === Set())
  }

  test("combinations: []") {
    assert(combinations(Nil) === List(Nil))
  }

  test("combinations: aa") {
    val aa = List(('a', 2))
    val aacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)))
    assert(combinations(aa).toSet === aacomb.toSet)
  }

  test("combinations: ab") {
    val ab = List(('a', 1), ('b', 1))
    val abcomb = List(
      List(),
      List(('a', 1)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)))
    assert(combinations(ab).toSet === abcomb.toSet)
  }

  test("combinations: aab") {
    val aab = List(('a', 2), ('b', 1))
    val aabcomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)))
    assert(combinations(aab).toSet === aabcomb.toSet)
  }

  test("combinations: ab vs ba") {
    val ab = List(('a', 1), ('b', 1))
    val ba = List(('b', 1), ('a', 1))
    assert(combinations(ab).toSet === combinations(ba).toSet)
  }

  test("combinations: abba") {
    val abba = List(('a', 2), ('b', 2))
    val abbacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 2)))

    assert(combinations(abba).toSet === abbacomb.toSet)
  }

  test("subtract: lard - r") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    assert(subtract(lard, r) === lad)
  }

  test("subtract: larrd - r") {
    val larrd = List(('a', 1), ('d', 1), ('l', 1), ('r', 2))
    val r = List(('r', 1))
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    assert(subtract(larrd, r) === lard)
  }

  test("sentence anagrams: []") {
    val sentence = List()
    assert(sentenceAnagrams(sentence) === List(Nil))
  }

  test("subset occ") {
    assert(occSubset(wordOccurrences("word"), wordOccurrences("wor")) == true)
    assert(occSubset(wordOccurrences("word"), wordOccurrences("wordw")) == false)
  }

  test("lengthOfOccurrence") {
    assert(lengthOfOccurrence(wordOccurrences("word")) == 4)
    assert(lengthOfOccurrence(wordOccurrences("wwoord")) == 6)
    assert(lengthOfOccurrence(Nil) == 0)
  }

  test("lengthOfSentence") {
    assert(lengthOfSentence(List("word", "wo")) == 6)
    assert(lengthOfSentence(List("wwoord")) == 6)
    assert(lengthOfSentence(List()) == 0)
  }

  test("sentence anagrams: Linux rulez") {
    val sentence = List("Linux", "rulez")
    val anas = List(
      List("Rex", "Lin", "Zulu"),
      List("nil", "Zulu", "Rex"),
      List("Rex", "nil", "Zulu"),
      List("Zulu", "Rex", "Lin"),
      List("null", "Uzi", "Rex"),
      List("Rex", "Zulu", "Lin"),
      List("Uzi", "null", "Rex"),
      List("Rex", "null", "Uzi"),
      List("null", "Rex", "Uzi"),
      List("Lin", "Rex", "Zulu"),
      List("nil", "Rex", "Zulu"),
      List("Rex", "Uzi", "null"),
      List("Rex", "Zulu", "nil"),
      List("Zulu", "Rex", "nil"),
      List("Zulu", "Lin", "Rex"),
      List("Lin", "Zulu", "Rex"),
      List("Uzi", "Rex", "null"),
      List("Zulu", "nil", "Rex"),
      List("rulez", "Linux"),
      List("Linux", "rulez"))
    assert(sentenceAnagrams(sentence).toSet === anas.toSet)
  }

  ignore("other sentences") {
    sentenceAnagrams(List("Lukas", "Rytz"))
    sentenceAnagrams(List("Yell", "Xerxes"))
  }

}