package ai.scala.fp.game.gomoku

import ai.scala.fp.game.mcts.MctsAi
import ai.scala.fp.geo.Pos
import com.truelaurel.codingame.time.Chronometer

import scala.concurrent.duration.DurationInt
import scala.util.Random

object GomokuDemo {
  def randomPlay(b: GomokuBoard): Pos = {
    val i = Random.nextInt(b.free.size)
    b.free.toVector(i)
  }

  val sortedPos = Seq(
    Pos(1, 1), // center
    Pos(0, 0), Pos(0, 2), Pos(2, 0), Pos(2, 2), // corners
    Pos(0, 1), Pos(1, 0), Pos(2, 1), Pos(1, 2)) // mid borders


  def smartMove(b: GomokuBoard): Pos =
    if (b.free.contains(Pos(1, 1))) Pos(1, 1) else randomPlay(b)

  // false starts
  val rules = GomokuRules(7, 5)

  def aiMove(s: GomokuBoard): Pos = {
    val chronometer = new Chronometer(2.seconds)
    chronometer.start()
    MctsAi(rules)(_ => chronometer.willOutOfTime).chooseMove(s)
  }

  def main(args: Array[String]): Unit = {
    val outcome = rules.judge(
      truePl = aiMove,
      falsePl = aiMove,
      s => println(s.toText))
    println(outcome)
  }
}
