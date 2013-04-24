package patmat

import common._

object Huffman {

  /**
   * A huffman code is represented by a binary tree.
   *
   * Every `Leaf` node of the tree represents one character of the alphabet that the tree can encode.
   * The weight of a `Leaf` is the frequency of appearance of the character.
   *
   * The branches of the huffman tree, the `Fork` nodes, represent a set containing all the characters
   * present in the leaves below it. The weight of a `Fork` node is the sum of the weights of these
   * leaves.
   */
  abstract class CodeTree { val weight: Int }
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree

  // Part 1: Basics

  def weight(tree: CodeTree): Int = tree match {
    case Leaf(_, weight) => weight
    case Fork(_, _, _, weight) => weight
  }

  def chars(tree: CodeTree): List[Char] = tree match {
    case Leaf(char, _) => List(char)
    case Fork(_, _, chars, _) => chars
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))

  // Part 2: Generating Huffman trees

  def string2Chars(str: String): List[Char] = str.toList

  /**
   * Returns the frequency of the occurence of Char in a List as pairs of (Char, Int)
   */
  def times(chars: List[Char]): List[(Char, Int)] = {

    def timesAcc(chars: List[Char], acc: List[(Char, Int)]): List[(Char, Int)] = {
      if (chars.isEmpty) {
        acc
      } else {
        val char = chars.head
        val pairs = splitList((p: (Char, Int)) => p._1 == char, acc)
        // acc is actually a map from Char=>Int
        val oldCount = if (pairs._1.isEmpty) 0 else pairs._1.head._2
        timesAcc(chars.tail, (char, oldCount + 1) :: pairs._2)
      }
    }
    timesAcc(chars, Nil)
  }

  /**
   * Split a list into a pair of two lists based on the pred function
   * pair._1 contains elements which satisfy pred(t)
   * pair._2 contains elements which satisfy !pred(t)
   */
  def splitList[T](pred: (T => Boolean), ts: List[T]): (List[T], List[T]) = {
    def splitListAcc(subTs: List[T], acc: List[T], rej: List[T]): (List[T], List[T]) = {
      if (subTs.isEmpty) {
        (acc, rej)
      } else {
        if (pred(subTs.head)) splitListAcc(subTs.tail, subTs.head :: acc, rej)
        else splitListAcc(subTs.tail, acc, subTs.head :: rej)
      }
    }
    splitListAcc(ts, Nil, Nil)
  }

  /**
   * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
   * ordered by ascending weights
   */
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
    val sortedFreq = freqs.sortBy(freq => freq._2)
    sortedFreq.map(freq => Leaf(freq._1, freq._2))
  }

  /**
   * Checks whether the list `trees` contains only one single code tree.
   */
  def singleton(trees: List[CodeTree]): Boolean = {
    !trees.isEmpty && trees.tail.isEmpty
  }

  /**
   * The parameter `trees` of this function is a list of code trees ordered
   * by ascending weights.
   *
   * This function takes the first two elements of the list `trees` and combines
   * them into a single `Fork` node. This node is then added back into the
   * remaining elements of `trees` at a position such that the ordering by weights
   * is preserved.
   *
   * If `trees` is a list of less than two elements, that list should be returned
   * unchanged.
   */
  def combine(trees: List[CodeTree]): List[CodeTree] = {

    if (trees.isEmpty) {
      Nil
    } else if (singleton(trees)) {
      trees
    } else {
      val first = trees.head
      val second = trees.tail.head
      val rest = trees.tail.tail

      val fork = Fork(first, second, chars(first) ++ chars(second), weight(first) + weight(second))
      (fork :: rest).sortWith((x, y) => x.weight < y.weight)
    }
  }

  /**
   * Method applies reduce to trees till check is not true
   * reduce() should minimize the number of elements in trees
   *
   */
  def until(check: (List[CodeTree] => Boolean), reduce: (List[CodeTree] => List[CodeTree]))(trees: List[CodeTree]): List[CodeTree] = {
    if (check(trees)) {
      trees
    } else {
      until(check, reduce)(reduce(trees))
    }
  }

  /**
   * This function creates a code tree which is optimal to encode the text `chars`.
   *
   * The parameter `chars` is an arbitrary text. This function extracts the character
   * frequencies from that text and creates a code tree based on them.
   */
  def createCodeTree(chars: List[Char]): CodeTree = {
    if (chars.isEmpty) {
      throw new IllegalArgumentException
    }
    val freqs = times(chars)
    val leaves = makeOrderedLeafList(freqs)
    val codeTree = until(singleton, combine)(leaves)
    codeTree.head
  }

  // Part 3: Decoding

  type Bit = Int

  /**
   * This function decodes the bit sequence `bits` using the code tree `tree` and returns
   * the resulting list of characters.
   */
  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {

    def decodeAcc(subTree: CodeTree, subBits: List[Bit], chars: List[Char]): List[Char] = {
      subTree match {
        case Leaf(char, _) => {
          val newChars = char :: chars
          if (subBits.isEmpty) {
            newChars
          } else {
            decodeAcc(tree, subBits, newChars)
          }
        }
        case Fork(left, right, _, _) => {
          val bit = subBits.head
          if (bit == 0) {
            decodeAcc(left, subBits.tail, chars)
          } else if (bit == 1) {
            decodeAcc(right, subBits.tail, chars)
          } else {
            throw new IllegalArgumentException
          }
        }
      }
    }
    tree match {
      // CodeTree with a single Leaf node, 0 or 1 map to the same char
      case Leaf(char, _) => bits.map(bit => char)
      // CodeTree with more than 1 levels 
      case Fork(_, _, _, _) => decodeAcc(tree, bits, Nil).reverse
    }
  }

  /**
   * A Huffman coding tree for the French language.
   * Generated from the data given at
   *   http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
   */
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s', 121895), Fork(Leaf('d', 56269), Fork(Fork(Fork(Leaf('x', 5928), Leaf('j', 8351), List('x', 'j'), 14279), Leaf('f', 16351), List('x', 'j', 'f'), 30630), Fork(Fork(Fork(Fork(Leaf('z', 2093), Fork(Leaf('k', 745), Leaf('w', 1747), List('k', 'w'), 2492), List('z', 'k', 'w'), 4585), Leaf('y', 4725), List('z', 'k', 'w', 'y'), 9310), Leaf('h', 11298), List('z', 'k', 'w', 'y', 'h'), 20608), Leaf('q', 20889), List('z', 'k', 'w', 'y', 'h', 'q'), 41497), List('x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 72127), List('d', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 128396), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 250291), Fork(Fork(Leaf('o', 82762), Leaf('l', 83668), List('o', 'l'), 166430), Fork(Fork(Leaf('m', 45521), Leaf('p', 46335), List('m', 'p'), 91856), Leaf('u', 96785), List('m', 'p', 'u'), 188641), List('o', 'l', 'm', 'p', 'u'), 355071), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u'), 605362), Fork(Fork(Fork(Leaf('r', 100500), Fork(Leaf('c', 50003), Fork(Leaf('v', 24975), Fork(Leaf('g', 13288), Leaf('b', 13822), List('g', 'b'), 27110), List('v', 'g', 'b'), 52085), List('c', 'v', 'g', 'b'), 102088), List('r', 'c', 'v', 'g', 'b'), 202588), Fork(Leaf('n', 108812), Leaf('t', 111103), List('n', 't'), 219915), List('r', 'c', 'v', 'g', 'b', 'n', 't'), 422503), Fork(Leaf('e', 225947), Fork(Leaf('i', 115465), Leaf('a', 117110), List('i', 'a'), 232575), List('e', 'i', 'a'), 458522), List('r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 881025), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u', 'r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 1486387)

  /**
   * What does the secret message say? Can you decode it?
   * For the decoding use the `frenchCode' Huffman tree defined above.
   */
  val secret: List[Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1)

  /**
   * Write a function that returns the decoded secret
   */
  def decodedSecret: List[Char] = decode(frenchCode, secret)

  // Part 4a: Encoding using Huffman tree

  /**
   * This function encodes `text` using the code tree `tree`
   * into a sequence of bits.
   */
  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    def encodeChar(subTree: CodeTree, char: Char, bits: List[Bit]): List[Bit] = {
      subTree match {
        case Leaf(_, _) => bits
        case Fork(left: Fork, right, _, _) => {
          if (left.chars.contains(char)) encodeChar(left, char, 0 :: bits)
          else encodeChar(right, char, 1 :: bits)
        }
        case Fork(left, right: Fork, _, _) => {
          if (right.chars.contains(char)) encodeChar(right, char, 1 :: bits)
          else encodeChar(left, char, 0 :: bits)
        }
        case Fork(left: Leaf, right: Leaf, _, _) => {
          if (left.char == char) encodeChar(left, char, 0 :: bits)
          else encodeChar(right, char, 1 :: bits)
        }
      }
    }
    tree match {
      // CodeTree with a single Leaf node, 0 or 1 map to the same char
      case Leaf(_, _) => text.map(char => 0)
      // CodeTree with more than 1 levels 
      case Fork(_, _, _, _) => text.map(char => encodeChar(tree, char, Nil).reverse).flatten
    }
  }

  // Part 4b: Encoding using code table

  type CodeTable = List[(Char, List[Bit])]

  /**
   * This function returns the bit sequence that represents the character `char` in
   * the code table `table`.
   */
  def codeBits(table: CodeTable)(char: Char): List[Bit] = table.find(code => code._1 == char).get._2

  /**
   * Given a code tree, create a code table which contains, for every character in the
   * code tree, the sequence of bits representing that character.
   *
   */
  def convert(tree: CodeTree): CodeTable = {
    def convertAcc(subTree: CodeTree, path: List[Bit], codeTable: CodeTable): CodeTable = {
      subTree match {
        case Leaf(char, _) => (char, path.reverse) :: codeTable
        case Fork(left, right, _, _) => {
          convertAcc(left, 0 :: path, codeTable) ++
            convertAcc(right, 1 :: path, codeTable)
        }
      }
    }
    convertAcc(tree, Nil, Nil)
  }

  /**
   * This function takes two code tables and merges them into one. Depending on how you
   * use it in the `convert` method above, this merge method might also do some transformations
   * on the two parameter code tables.
   */
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = ???

  /**
   * This function encodes `text` according to the code tree `tree`.
   *
   * To speed up the encoding process, it first converts the code tree to a code table
   * and then uses it to perform the actual encoding.
   */
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    val codeBitProvider = codeBits(convert(tree))_
    text.map(char => codeBitProvider(char)).flatten
  }
}
