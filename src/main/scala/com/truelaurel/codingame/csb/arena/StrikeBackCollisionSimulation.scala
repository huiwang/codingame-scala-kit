package com.truelaurel.codingame.csb.arena

import com.truelaurel.codingame.collision.Collision
import com.truelaurel.codingame.csb.model.{CheckPoint, Pod}


object StrikeBackCollisionSimulation {

  def simulate(checkPoints: Vector[CheckPoint], pods: Vector[Pod], duration: Double): Vector[Pod] = {
    val podCpCollisions = for {
      pod <- pods
      cp <- checkPoints
      if pod.goal % checkPoints.size == cp.id
      time <- Collision.collideTime(pod.position, pod.speed, pod.radius, cp.position, cp.speed, cp.radius)
      if time < duration
    } yield (pod, cp, time)

    val podPodCollisions = for {
      Vector(pod1, pod2) <- pods.combinations(2)
      time <- Collision.collideTime(pod1.position, pod1.speed, pod1.radius, pod2.position, pod2.speed, pod2.radius)
      if time < duration
    } yield (pod1, pod2, time)

    if (podCpCollisions.isEmpty && podPodCollisions.isEmpty) {
      pods.map(movePod(_, duration))
    } else if (podCpCollisions.isEmpty) {
      val (pod1, pod2, time) = podPodCollisions.minBy(_._3)
      simulate(checkPoints, movePodPodCollision(pods, pod1, pod2, time), duration - time)
    } else if (podPodCollisions.isEmpty) {
      val (pod, cp, time) = podCpCollisions.minBy(_._3)
      simulate(checkPoints, movePodCpCollision(pods, pod, time), duration - time)
    } else {
      val (pod1, pod2, timePodPod) = podPodCollisions.minBy(_._3)
      val (pod, cp, timePodCp) = podCpCollisions.minBy(_._3)
      if (timePodPod < timePodCp) {
        simulate(checkPoints, movePodPodCollision(pods, pod1, pod2, timePodPod), duration - timePodPod)
      } else {
        simulate(checkPoints, movePodCpCollision(pods, pod, timePodCp), duration - timePodCp)
      }
    }
  }

  private def movePodCpCollision(pods: Vector[Pod], pod: Pod, time: Double) = {
    pods.map(p => if (p.id == pod.id) movePodAndGoal(p, time) else movePod(p, time))
  }

  private def movePodPodCollision(pods: Vector[Pod], pod1: Pod, pod2: Pod, time: Double) = {
    val (bouncedPod1, bouncedPod2) = boundOff(pod1, pod2, time)
    pods.map(p => p.id match {
      case pod1.id => bouncedPod1
      case pod2.id => bouncedPod2
      case _ => movePod(p, time)
    })
  }

  private def boundOff(pod1: Pod, pod2: Pod, time: Double) = {
    val movedPod1 = movePod(pod1, time)
    val movedPod2 = movePod(pod2, time)
    val (speed1, speed2) = Collision.bounceOffWithMinimumImpulse(
      movedPod1.position, movedPod1.speed, movedPod1.radius,
      movedPod2.position, movedPod2.speed, movedPod2.radius,
      120.0
    )
    val bouncedPod1 = movedPod1.copy(speed = speed1)
    val bouncedPod2 = movedPod2.copy(speed = speed2)
    (bouncedPod1, bouncedPod2)
  }

  private def movePod(pod: Pod, duration: Double) = {
    pod.copy(position = pod.position + (pod.speed * duration))
  }

  private def movePodAndGoal(pod: Pod, duration: Double) = {
    pod.copy(position = pod.position + (pod.speed * duration), goal = pod.goal + 1)
  }

}
