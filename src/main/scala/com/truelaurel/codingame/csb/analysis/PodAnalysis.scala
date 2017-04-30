package com.truelaurel.codingame.csb.analysis

import com.truelaurel.codingame.collision.Collision
import com.truelaurel.codingame.csb.model.Pod

/**
  * Created by hwang on 30/04/2017.
  */
object PodAnalysis {

  def collisionTime(pod1: Pod, pod2: Pod): Option[Double] = {
    Collision.collideTime(pod1.position, pod1.speed, pod1.radius, pod2.position, pod2.speed, pod2.radius)
  }
}
