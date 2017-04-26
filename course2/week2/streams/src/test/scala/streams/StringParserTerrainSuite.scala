package streams

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class StringParserTerrainSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
      * This method applies a list of moves `ls` to the block at position
      * `startPos`. This can be used to verify if a certain list of moves
      * is a valid solution, i.e. leads to the goal.
      */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) =>
        require(block.isLegal) // The solution must always lead to legal blocks
        move match {
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
      """------
        |--ST--
        |--oo--
        |--oo--
        |------""".stripMargin
  }

  trait Level2 extends SolutionChecker {
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


  test("neighborsWithHistory") {
    new Level2 {

      assert(Block(Pos(1, 1), Pos(1, 1)).legalNeighbors.length == 2)

      assert(neighborsWithHistory(Block(Pos(1, 1), Pos(1, 1)), List(Left, Up)).toSet === Set(
        (Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))))
    }
  }



	test("terrain function level 1") {
    new Level1 {
      assert(!terrain(Pos(0,0)), "0,0")
      assert(!terrain(Pos(1,1)), "1,1")
      assert(!terrain(Pos(4,7)), "4,7")

      assert(!terrain(Pos(5,8)), "5,8")
      assert(!terrain(Pos(5,9)), "5,9")

      assert(terrain(Pos(1,2)), "1,2") // start
      assert(terrain(Pos(1,3)), "1,3") // goal

      assert(terrain(Pos(3,2)), "3,2")
      assert(terrain(Pos(3,3)), "3,3")
    }
  }

  test("findChar level 1 - start") {
    new Level1 {
      assert(startPos == Pos(1,2))
    }
  }


  test("findChar level 1 - goal") {
    new Level1 {
      assert(goal == Pos(1,3))
    }
  }


  test("isStanding - goal") {
    new Level1 {
      assert(Block(Pos(1,3), Pos(1,3)).isStanding)
      assert(!Block(Pos(1,3), Pos(2,3)).isStanding)
      assert(!Block(Pos(2,3), Pos(2,4)).isStanding)
    }
  }

  test("isLegal") {
    new Level1 {
      assert(Block(Pos(1,3), Pos(2,3)).isLegal)
      assert(Block(Pos(2,2), Pos(2,3)).isLegal)

      assert(!Block(Pos(0,3), Pos(1,3)).isLegal)
      assert(!Block(Pos(3,3), Pos(4,3)).isLegal)
      assert(!Block(Pos(2,3), Pos(2,4)).isLegal)
    }
  }



}
