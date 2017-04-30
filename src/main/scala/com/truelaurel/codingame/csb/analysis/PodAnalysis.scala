package com.truelaurel.codingame.csb.analysis

import com.truelaurel.codingame.collision.Collision
import com.truelaurel.codingame.csb.model.{Pod, StrikeBackState}

/**
  * Created by hwang on 30/04/2017.
  */
object PodAnalysis {

  def collisionTime(pod1: Pod, pod2: Pod): Option[Double] = {
    Collision.collideTime(pod1.position, pod1.speed, pod1.radius, pod2.position, pod2.speed, pod2.radius)
  }

  def distance(pod1: Pod, pod2: Pod) : Double = (pod1.position - pod2.position).mag

  def distanceToGoal(pod: Pod, podWithGoal : Pod, state : StrikeBackState): Double = {
    (pod.position - state.checkPoint(podWithGoal.goal)).mag
  }

  def distanceToNextGoal(pod: Pod, goal : Int, state : StrikeBackState): Double = {
    (pod.position - state.checkPoint(goal + 1)).mag
  }
}
