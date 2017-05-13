package com.truelaurel.codingame.csb.analysis

import com.truelaurel.codingame.collision.Collision
import com.truelaurel.codingame.csb.model.{CheckPoint, Pod, StrikeBackState}

/**
  * Created by hwang on 30/04/2017.
  */
object PodAnalysis {

  def podToPodCollisionTime(pod1: Pod, pod2: Pod): Option[Double] = {
    Collision.collideTime(pod1.position, pod1.speed, pod1.radius, pod2.position, pod2.speed, pod2.radius)
  }

  def podToGoalCollisionTime(pod: Pod, state: StrikeBackState): Double = {
    val goal = goalOf(pod, state)
    Collision.collideTime(pod.position, pod.speed, pod.radius,
      goal.position, goal.speed, goal.radius).getOrElse(Double.MaxValue)
  }

  def distance(pod1: Pod, pod2: Pod): Double = (pod1.position - pod2.position).mag

  def podToOtherGoalDistance(pod: Pod, podWithGoal: Pod, state: StrikeBackState): Double = {
    (pod.position - goalOf(podWithGoal, state).position).mag
  }

  def podToGoalDistance(pod: Pod, state: StrikeBackState): Double = {
    (pod.position - goalOf(pod, state).position).mag
  }

  def podTodNextGoalDistance(pod: Pod, goal: Int, state: StrikeBackState): Double = {
    (pod.position - state.context.checkPoints((goal + 1) % state.context.checkPoints.size).position).mag
  }

  def goalOf(pod: Pod, state: StrikeBackState): CheckPoint = {
    state.context.checkPoints(pod.goal % state.context.checkPoints.size)
  }

  def pivotTo(defense: Pod, racer: Pod): Double = {
    defense.angle.dotProduct((racer.position - defense.position).norm)
  }


}
