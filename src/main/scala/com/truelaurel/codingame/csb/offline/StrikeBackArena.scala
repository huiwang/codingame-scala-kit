package com.truelaurel.codingame.csb.offline

import com.truelaurel.codingame.collision._
import com.truelaurel.codingame.csb.common._
import com.truelaurel.codingame.engine.{GameArena, GameResult}
import com.truelaurel.codingame.vectorial.Vectorl

/**
  * Created by hwang on 02/04/2017.
  */
object StrikeBackArena extends GameArena[StrikeBackGameState, PodAction] {

  val aixX = Vectorl(1, 0)
  private val simulation = new CollisionSimulation[Any](DiskVirtualDiskCollider)

  override def next(fromState: StrikeBackGameState, actions: Vector[PodAction]): StrikeBackGameState = {
    val updatedPods = actions.indices.map(i => {
      val pod = fromState.pods(i)
      actions(i) match {
        case Thrust(target, thrust) =>
          pod.copy(disk = pod.disk.copy(v = acceleratedVitess(pod, target, thrust)))
        case Shield => pod
        case Boost => pod
      }
    })


    val collidables = updatedPods ++ fromState.checkPoints

    val results = simulation.simulate(collidables.toVector)

    val simulated = results.take(fromState.pods.size)
    val pods = simulated.map {
      case pod : Pod =>
        pod
      case _ => throw new IllegalArgumentException
    }
    StrikeBackGameState(fromState.checkPoints, pods.map(p => p.copy(disk = p.disk.copy(v = p.disk.v * 0.85))))
  }

  private def acceleratedVitess(pod: Pod, target: Vectorl, thrust: Int) = {
    val position = pod.disk.p
    val facing = aixX.rotateInDegree(pod.angle)
    val desired = target - position
    val pivoted = if (pod.angle == -1 || facing.angleInDegreeBetween(desired) <= 18) {
      desired
    } else {
      if (facing.perDotProduct(desired) > 0) {
        facing.rotateInDegree(18)
      } else {
        facing.rotateInDegree(-18)
      }
    }
    pod.disk.v + pivoted.norm * thrust
  }

  override def judge(state: StrikeBackGameState): GameResult = {
    ???
  }
}

case object DiskVirtualDiskCollider extends Collider[Any] {
  override def collideTime(c1: Any, c2: Any): Option[Double] = (c1, c2) match {
    case (d1: Pod, d2: Pod) => DiskCollider.collideTime(d1.disk, d2.disk)
    case (d1: Pod, d2: CheckPoint) => DiskCollider.collideTime(d1.disk, d2.disk).filter(_ > 0)
    case (d1: CheckPoint, d2: Pod) => DiskCollider.collideTime(d1.disk, d2.disk).filter(_ > 0)
    case _ => None
  }

  override def bounceOff(c1: Any, c2: Any): (Any, Any) = (c1, c2) match {
    case (d1: Pod, d2: Pod) =>
      val pair = DiskCollider.bounceOff(d1.disk, d2.disk)
      (d1.copy(disk = pair._1), d2.copy(disk = pair._2))
    case _ => (c1, c2)
  }

  override def move(collidable: Any, time: Double): Any = collidable match {
    case d: Pod => d.copy(disk = DiskMover.move(d.disk, time))
    case _ => collidable
  }
}


