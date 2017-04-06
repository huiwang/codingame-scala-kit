package com.truelaurel.codingame.csb.offline

import com.truelaurel.codingame.collision._
import com.truelaurel.codingame.csb.common._
import com.truelaurel.codingame.engine.{GameArena, GameResult}
import com.truelaurel.codingame.vectorial.Vectorl

/**
  * Created by hwang on 02/04/2017.
  */
object StrikeBackArena extends GameArena[StrikeBackGameState, PodAction] {

  private val simulation = new CollisionSimulation[Disk](DiskVirtualDiskCollider)

  override def next(fromState: StrikeBackGameState, actions: Vector[PodAction]): StrikeBackGameState = {
    val updatedPods = actions.indices.map(i => {
      val pod = fromState.pods(i)
      val angle = fromState.angles(i)
      actions(i) match {
        case Thrust(target, thrust) =>
          val pivoted = angle.pivotTo(target - pod.p, 18)
          (pod.copy(v = pod.v + pivoted * thrust), pivoted)
        case Shield => (pod, angle)
        case Boost => (pod, angle)
      }
    }).toVector

    val pods = updatedPods.map(_._1)
    val angles: Vector[Vectorl] = updatedPods.map(_._2)
    val simulated = simulation.simulate(pods ++ fromState.checkPoints).take(fromState.pods.size)
    StrikeBackGameState(
      fromState.checkPoints,
      simulated.map(p => p.copy(v = p.v * 0.85)),
      angles,
      fromState.nextCP
    )
  }

  override def judge(state: StrikeBackGameState): GameResult = {
    ???
  }
}

case object DiskVirtualDiskCollider extends Collider[Disk] {
  override def collideTime(c1: Disk, c2: Disk): Option[Double] = DiskCollider.collideTime(c1, c2)

  override def bounceOff(c1: Disk, c2: Disk): (Disk, Disk) = {
    if (c1.r == CSBConstant.cpRadius || c2.r == CSBConstant.cpRadius)
      (c1, c2) else DiskCollider.bounceOff(c1, c2)
  }

  override def move(collidable: Disk, time: Double): Disk = {
    if (collidable.r == CSBConstant.cpRadius) collidable else {
      DiskMover.move(collidable, time)
    }
  }
}


