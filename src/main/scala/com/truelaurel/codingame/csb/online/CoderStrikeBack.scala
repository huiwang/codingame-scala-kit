import com.truelaurel.codingame.collision.Disk
import com.truelaurel.codingame.csb.common.{CheckPoint, Pod, StrikeBackGameState}
import com.truelaurel.codingame.csb.head.StrikeBackPlayer
import com.truelaurel.codingame.csb.offline.StrikeBackArena
import com.truelaurel.codingame.engine.GameSimulator
import com.truelaurel.codingame.vectorial.Vectorl

import scala.io.StdIn

object Player extends App {
  val laps = StdIn.readInt()
  val checkpointCount = StdIn.readInt()
  val checkpoints = 0.until(checkpointCount).map(i => {
    val Array(x, y) = StdIn.readLine().split(" ").map(_.toInt)
    CheckPoint(Disk(Vectorl(x, y), Vectorl(0, 0), 200))
  }).toVector

  val player = StrikeBackPlayer(0)
  var predicated: StrikeBackGameState = null
  while (true) {

    val pods = checkpointCount.until(checkpointCount + 4).map(i => {
      val Array(x, y, vx, vy, angle, nextcheckpointid) = StdIn.readLine().split(" ").map(_.toInt)
      Pod(Disk(Vectorl(x, y), Vectorl(vx, vy), 400), angle, nextcheckpointid)
    }).toVector
    val state = StrikeBackGameState(checkpoints, pods)
    if (predicated != null) {
      System.err.println(predicated)
    }
    System.err.println(state)
    predicated = GameSimulator.simulate(1, state, StrikeBackArena, Vector(
      StrikeBackPlayer(0), StrikeBackPlayer(1)
    ))
    val actions = player.reactTo(state)
    actions.foreach(a => println(a.command()))
  }

}