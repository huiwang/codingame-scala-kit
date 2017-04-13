package com.truelaurel.codingame.csb.offline

import com.truelaurel.codingame.collision._
import com.truelaurel.codingame.csb.best.BestStrikeBackPlayer
import com.truelaurel.codingame.csb.common._
import com.truelaurel.codingame.csb.head.StrikeBackPlayer
import com.truelaurel.codingame.engine.{Draw, GameArena, GameResult, GameSimulator}
import com.truelaurel.codingame.vectorial.Vectorl

/**
  * Created by hwang on 02/04/2017.
  */
object StrikeBackArena extends GameArena[StrikeBackGameState, PodAction] {

  override def next(fromState: StrikeBackGameState, actions: Vector[PodAction]): StrikeBackGameState = {
    require(actions.size == 4)
    val podsAndAngles = CSBConstant.podIndices.map(i => {
      val pod = fromState.pods(i)
      val angle = fromState.angles(i)
      actions(i) match {
        case Thrust(target, thrust) =>
          val pivoted = angle.pivotTo(target - pod.p, 18.0)
          (pod.copy(v = pod.v + pivoted * thrust), pivoted)
        case AngleThrust(_, _, rotate, thrust) =>
          val rotated = angle.rotateInDegree(rotate)
          (pod.copy(v = pod.v + rotated * thrust), rotated)
        case Shield => (pod, angle)
        case Boost => (pod, angle)
      }
    })

    val collidables = podsAndAngles.map(_._1) ++ fromState.checkPoints
    val (collidedPods, collisions) = new CollisionSimulation[Disk](DiskVirtualDiskCollider).simulate(collidables)
    val collisionWithCp = collisions.filter(e => collidables(e.id2).r == CSBConstant.cpRadius)

    val nextCps = for {
      i <- CSBConstant.podIndices
      betweenPodAndCP = collisionWithCp.filter(_.id1 == i)
      next = nextCp(fromState.nextCPs(i), betweenPodAndCP)
    } yield next

    StrikeBackGameState(
      fromState.checkPoints,
      collidedPods.take(CSBConstant.podCount).map(pod => pod.copy(v = (pod.v * 0.85).truncate, p = pod.p.round)),
      podsAndAngles.map(_._2),
      nextCps
    )
  }

  def nextCp(current: Int, betweenPodAndCP: Vector[CollisionEvent]): Int = {
    if (betweenPodAndCP.isEmpty) current
    else {
      val headCollision = betweenPodAndCP.head
      if (headCollision.id2 == current + CSBConstant.podCount) {
        nextCp(current + 1, betweenPodAndCP.tail)
      } else {
        current
      }
    }
  }


  override def judge(state: StrikeBackGameState): GameResult = {
    Draw
  }

  def main(args: Array[String]): Unit = {
    val state = StrikeBackGameState(Vector(Disk(Vectorl(13930.0,1925.0),Vectorl(0.0,0.0),200.0,1.0), Disk(Vectorl(8026.0,3273.0),Vectorl(0.0,0.0),200.0,1.0), Disk(Vectorl(2662.0,6991.0),Vectorl(0.0,0.0),200.0,1.0), Disk(Vectorl(10020.0,5967.0),Vectorl(0.0,0.0),200.0,1.0)),Vector(Disk(Vectorl(14041.0,2412.0),Vectorl(0.0,0.0),400.0,1.0), Disk(Vectorl(13819.0,1438.0),Vectorl(0.0,0.0),400.0,1.0), Disk(Vectorl(14264.0,3387.0),Vectorl(0.0,0.0),400.0,1.0), Disk(Vectorl(13596.0,463.0),Vectorl(0.0,0.0),400.0,1.0)),Vector(Vectorl(-0.9899099573973741,0.14169783430077126), Vectorl(-0.9533161724973302,0.3019739645317799), Vectorl(-0.9998330523927659,-0.018272037187043174), Vectorl(-0.8928185400193172,0.4504165345519356)),Vector(1, 1, 1, 1))
    val players = Vector(StrikeBackPlayer(Vector(0, 1), Vector(2, 3)), BestStrikeBackPlayer(Vector(2, 3)))
    GameSimulator.evaluateOffline(Vector(state), StrikeBackArena, players)
  }

}

case object DiskVirtualDiskCollider extends Collider[Disk] {
  override def collideTime(c1: Disk, c2: Disk): Option[Double] =
    DiskCollider.collideTime(c1, c2).filter(t => t > 0)

  override def bounceOff(c1: Disk, c2: Disk): (Disk, Disk) = {
    if (c1.r == CSBConstant.cpRadius || c2.r == CSBConstant.cpRadius)
      (c1, c2)
    else {
      bounceOffPods(c1, c2)
    }
  }

  def bounceOffPods(d1: Disk, d2: Disk): (Disk, Disk) = {
    val dr = d2.p - d1.p
    val dv = d2.v - d1.v
    val drdr = dr.dotProduct(dr)
    val dvdr = dr.dotProduct(dv)
    val massCoefficient = (d1.m + d2.m) / (d1.m * d2.m)

    val impulse = dr * 2.0 * dvdr / (massCoefficient * drdr)
    val adjusted = impulse.norm * (impulse.mag * 0.5 + 120.0).max(impulse.mag)

    (d1.copy(v = d1.v + adjusted / d1.m), d2.copy(v = d2.v - adjusted / d2.m))
  }

  override def move(collidable: Disk, time: Double): Disk = {
    if (collidable.r == CSBConstant.cpRadius) collidable
    else {
      DiskMover.move(collidable, time)
    }
  }
}


