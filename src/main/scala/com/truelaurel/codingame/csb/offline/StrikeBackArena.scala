package com.truelaurel.codingame.csb.offline

import com.truelaurel.codingame.collision._
import com.truelaurel.codingame.csb.common._
import com.truelaurel.codingame.engine.{GameArena, GameResult}

/**
  * Created by hwang on 02/04/2017.
  */
object StrikeBackArena extends GameArena[StrikeBackGameState, PodAction] {

  private val simulation = new CollisionSimulation[Disk](DiskVirtualDiskCollider)

  override def next(fromState: StrikeBackGameState, actions: Vector[PodAction]): StrikeBackGameState = {
    val podsAndAngles = CSBConstant.podIndices.map(i => {
      val pod = fromState.pods(i)
      val angle = fromState.angles(i)
      actions(i) match {
        case Thrust(target, thrust) =>
          val pivoted = angle.pivotTo(target - pod.p, 18)
          (pod.copy(v = pod.v + pivoted * thrust), pivoted)
        case Shield => (pod, angle)
        case Boost => (pod, angle)
      }
    })

    val collidables = podsAndAngles.map(_._1) ++ fromState.checkPoints
    val (collidedPods, collisions) = simulation.simulate(collidables)
    val collisionWithCp = collisions.filter(e => collidables(e.id2).r == CSBConstant.cpRadius)

    val nextCps = for {
      i <- CSBConstant.podIndices
      betweenPodAndCP =  collisionWithCp.filter(_.id1 == i)
      next = nextCp(fromState.nextCPs(i), betweenPodAndCP)
    } yield next
    StrikeBackGameState(
      fromState.checkPoints,
      collidedPods.take(4).map(p => p.copy(v = p.v * 0.85)),
      podsAndAngles.map(_._2),
      nextCps
    )
  }

  def nextCp(current: Int, collisionsWithCP: Vector[CollisionEvent]): Int = {
    if (collisionsWithCP.isEmpty) current
    else {
      val headCollision = collisionsWithCP.head
      if (headCollision.id2 == current + 1) {
        nextCp(current + 1, collisionsWithCP.tail)
      } else {
        current
      }
    }
  }


  override def judge(state: StrikeBackGameState): GameResult = {
    ???
  }
}

case object DiskVirtualDiskCollider extends Collider[Disk] {
  override def collideTime(c1: Disk, c2: Disk): Option[Double] = DiskCollider.collideTime(c1, c2)

  override def bounceOff(c1: Disk, c2: Disk): (Disk, Disk) = {
    if (c1.r == CSBConstant.cpRadius || c2.r == CSBConstant.cpRadius)
      (c1, c2)
    else DiskCollider.bounceOff(c1, c2)
  }

  override def move(collidable: Disk, time: Double): Disk = {
    if (collidable.r == CSBConstant.cpRadius) collidable
    else {
      DiskMover.move(collidable, time)
    }
  }
}


