import com.truelaurel.codingame.collision.Disk
import com.truelaurel.codingame.csb.common.{CSBConstant, CheckPoint, Pod, StrikeBackGameState}
import com.truelaurel.codingame.csb.head.StrikeBackPlayer
import com.truelaurel.codingame.csb.offline.StrikeBackArena
import com.truelaurel.codingame.engine.GameSimulator
import com.truelaurel.codingame.vectorial.{Vectorl, Vectorls}

import scala.io.StdIn

object Player extends App {
  val laps = StdIn.readInt()
  val checkpointCount = StdIn.readInt()
  val checkpoints = 0.until(checkpointCount).map(i => {
    val Array(x, y) = StdIn.readLine().split(" ").map(_.toInt)
    Disk(Vectorl(x, y), Vectorl(0, 0), CSBConstant.cpRadius)
  }).toVector

  val player = StrikeBackPlayer(Vector(0, 1))
  var predicated: StrikeBackGameState = null
  while (true) {

    val pods = checkpointCount.until(checkpointCount + 4).map(i => {
      val Array(x, y, vx, vy, angle, nextcheckpointid) = StdIn.readLine().split(" ").map(_.toInt)
      (Disk(Vectorl(x, y), Vectorl(vx, vy), 400), angle, nextcheckpointid)
    }).toVector
    val disks: Vector[Disk] = pods.map(_._1)
    val angles: Vector[Vectorl] = pods.map(_._2)
      .map(angle => if (angle == -1) Vectorls.origin else Vectorls.axisX.rotateInDegree(angle))
    val nextCheck: Vector[Int] = pods.map(_._3)
    val state = StrikeBackGameState(checkpoints, disks, angles, nextCheck)
    if (predicated != null) {
      System.err.println(predicated)
    }
    System.err.println(state)
    predicated = GameSimulator.simulate(1, state, StrikeBackArena, Vector(
      StrikeBackPlayer(Vector(0, 1)), StrikeBackPlayer(Vector(2, 3))
    ))
    val actions = player.reactTo(state)
    actions.foreach(a => println(a.command()))
  }

}