import com.truelaurel.codingame.csb.common.{CheckPoint, Pod, StrikeBackGameState}
import com.truelaurel.codingame.csb.head.StrikeBackPlayer

import scala.io.StdIn

/**
  * Auto-generated code below aims at helping you parse
  * the standard input according to the problem statement.
  **/
object Player extends App {
  val laps = StdIn.readInt()
  val checkpointCount = StdIn.readInt()
  val checkpoints = 1.to(checkpointCount).map(_ => {
      val Array(x, y) = StdIn.readLine().split(" ").map(_.toInt)
      CheckPoint(x, y)
    }).toVector

  val player = StrikeBackPlayer(0)
  while(true) {
    val pods0 = readPods
    val pods1 = readPods
    val state = StrikeBackGameState(checkpoints, Vector(pods0, pods1))
    val actions = player.reactTo(state)
    actions.foreach(a => println(a.command()))
  }

  private def readPods = {
    (for {
      i <- 0 until 2
      Array(x, y, vx, vy, angle, nextcheckpointid) = StdIn.readLine().split(" ").map(_.toInt)
    } yield Pod(x, y, vx, vy, angle, nextcheckpointid)).toVector
  }
}