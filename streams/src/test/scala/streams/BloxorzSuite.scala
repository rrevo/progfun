package streams

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  test("string parser") {
    class TestParserTerrain(stringLevel: String) extends GameDef with StringParserTerrain {
      val level = stringLevel

      def valid(x: Int, y: Int): Boolean = {
        terrain(Pos(x, y))
      }
    }
    val level =
      """------
        |--ST--
        |--oo--
        |--oo--
        |------""".stripMargin
    val terrain = new TestParserTerrain(level)
    assert(terrain.valid(1, 2))
    assert(terrain.valid(1, 3))
    assert(terrain.valid(2, 3))
    assert(!terrain.valid(0, 0))
    assert(!terrain.valid(3, 4))
    assert(!terrain.valid(4, 5))
    assert(!terrain.valid(-1, 0))
    assert(!terrain.valid(100, 0))

    assert(terrain.startPos.x == 1)
    assert(terrain.startPos.y == 2)
    assert(terrain.goal.x == 1)
    assert(terrain.goal.y == 3)
  }

  test("blocks test") {
    class TestGame(stringLevel: String) extends GameDef with StringParserTerrain {
      val level = stringLevel
    }
    def blockAt(move: game.Move, ms: Map[game.Move, List[(game.Block, game.Move)]]): game.Block = {
      ms.get(move).get.head._1
    }
    val level =
      """------
        |--SoT-
        |--ooo-
        |--oo--
        |------""".stripMargin
    val game = new TestGame(level);
    {
      val block = game.startBlock;
      {
        val ns = block.neighbors.groupBy(n => n._2)
        assert(ns.size == 4)
        assert(blockAt(game.Left, ns) == game.Block(game.Pos(1, 0), game.Pos(1, 1)))
        assert(blockAt(game.Right, ns) == game.Block(game.Pos(1, 3), game.Pos(1, 4)))
        assert(blockAt(game.Up, ns) == game.Block(game.Pos(-1, 2), game.Pos(0, 2)))
        assert(blockAt(game.Down, ns) == game.Block(game.Pos(2, 2), game.Pos(3, 2)))
      }
      {
        val legalNs = block.legalNeighbors.groupBy(n => n._2)
        assert(legalNs.size == 2)
        assert(blockAt(game.Down, legalNs) == game.Block(game.Pos(2, 2), game.Pos(3, 2)))
        assert(blockAt(game.Right, legalNs) == game.Block(game.Pos(1, 3), game.Pos(1, 4)))
      }
    }
    {
      val block = game.Block(game.Pos(2, 2), game.Pos(2, 3));
      {
        val ns = block.neighbors.groupBy(n => n._2)
        assert(ns.size == 4)
        assert(blockAt(game.Left, ns) == game.Block(game.Pos(2, 1), game.Pos(2, 1)))
        assert(blockAt(game.Right, ns) == game.Block(game.Pos(2, 4), game.Pos(2, 4)))
        assert(blockAt(game.Up, ns) == game.Block(game.Pos(1, 2), game.Pos(1, 3)))
        assert(blockAt(game.Down, ns) == game.Block(game.Pos(3, 2), game.Pos(3, 3)))
      }
      {
        val legalNs = block.legalNeighbors.groupBy(n => n._2)
        assert(legalNs.size == 3)
        assert(blockAt(game.Right, legalNs) == game.Block(game.Pos(2, 4), game.Pos(2, 4)))
        assert(blockAt(game.Up, legalNs) == game.Block(game.Pos(1, 2), game.Pos(1, 3)))
        assert(blockAt(game.Down, legalNs) == game.Block(game.Pos(3, 2), game.Pos(3, 3)))
      }
    }
  }

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) {
        case (block, move) => move match {
          case Left => block.left
          case Right => block.right
          case Up => block.up
          case Down => block.down
        }
      }
  }

  trait Level1 extends SolutionChecker {
    /* terrain for level 1*/

    val level =
      """ooo-------
        |oSoooo----
        |ooooooooo-
        |-ooooooooo
        |-----ooToo
        |------ooo-""".stripMargin
    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0, 0)), "0,0")
      assert(!terrain(Pos(4, 11)), "4,11")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1, 1))
    }
  }

  test("neighborsWithHistory") {
    new Level1 {
      val ns = neighborsWithHistory(Block(Pos(1, 1), Pos(1, 1)), List(Left, Up)).toMap
      assert(ns.size == 2)
      assert(ns.get(Block(Pos(1, 2), Pos(1, 3))).get == List(Right, Left, Up))
      assert(ns.get(Block(Pos(2, 1), Pos(3, 1))).get == List(Down, Left, Up))
    }
  }

  test("newNeighborsOnly") {
    new Level1 {
      val ns = newNeighborsOnly(
        Set(
          (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
          (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))).toStream,

        Set(Block(Pos(1, 2), Pos(1, 3)), Block(Pos(1, 1), Pos(1, 1)))).toMap
      assert(ns.size == 1)
      assert(ns.get(Block(Pos(2, 1), Pos(3, 1))).get == List(Down, Left, Up))
    }
  }

  test("from empty") {
    new Level1 {
      val noPath = from(Stream[(Block, List[Move])](), Set[Block]())
      assert(noPath.isEmpty)
    }
  }

  test("from") {
    new Level1 {
      var path = pathsFromStart
      val firstPath = path

      assert(path.size == 2)
      var curr = path.head
      val firstRight = curr
      path = path.tail
      assert(curr == (Block(Pos(1, 2), Pos(1, 3)), List(Right)))
      curr = path.head
      val firstDown = curr
      path = path.tail
      assert(curr == (Block(Pos(2, 1), Pos(3, 1)), List(Down)))

      path = from(firstPath, Set(startBlock))
      assert(path.size == 3)
      curr = path.head
      path = path.tail
      assert(curr == firstDown)
      curr = path.head
      path = path.tail
      assert(curr == (Block(Pos(1, 4), Pos(1, 4)), List(Right, Right)))
      curr = path.head
      path = path.tail
      assert(curr == (Block(Pos(2, 2), Pos(2, 3)), List(Down, Right)))
    }
  }

  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
      assert(solution.length == optsolution.length)
    }
  }

  trait LevelWithMultipleSolutions extends SolutionChecker {
    /* terrain for level 2*/

    val level =
      """ooo--
        |oSooT
        |ooooo
        |-oooo""".stripMargin
    val solutions = List(List(Right, Right), List(Down, Right, Right, Right, Up))
  }

  test("solutions for LevelWithMultipleSolutions") {
    new LevelWithMultipleSolutions {
      val foundSolutions = pathsToGoal.map(t => t._2.reverse)
      assert(foundSolutions == solutions)
    }
  }
}
