package com.truelaurel.codingame.collision

import scala.collection.mutable

trait Collidable {
}

trait Collider[T] {

  def collideTime(c1: T, c2: T): Option[Double]

  def bounceOff(c1: T, c2: T): (T, T)

  def move(collidable: T, time: Double): T
}

case class CollisionEvent(collisionTime: Double, id1: Int, hits1: Int, id2: Int, hits2: Int) extends Ordered[CollisionEvent] {

  override def compare(that: CollisionEvent): Int = that.collisionTime.compare(collisionTime)
}

class CollisionSimulation[T](val collider: Collider[T], val duration: Double = 1.0) {

  private val pq = new mutable.PriorityQueue[CollisionEvent]
  private val hits = new mutable.HashMap[Int, Int]()
  private val current = new mutable.HashMap[Int, T]()

  def simulate(collidables: Vector[T]): Vector[T] = {
    collidables.indices.foreach(i => current(i) = collidables(i))
    for (i <- collidables.indices) {
      for (j <- (i + 1) until collidables.size) {
        predict(i, j, 0)
      }
    }
    var lastCollisionTime = 0.0
    while (pq.nonEmpty) {
      val event = pq.dequeue()
      if (isEventValid(event) && event.collisionTime < duration) {
        current.transform((_, c) => collider.move(c, event.collisionTime - lastCollisionTime))
        val (c1, c2) = collider.bounceOff(current(event.id1), current(event.id2))
        hits.put(event.id1, hits(event.id1) + 1)
        hits.put(event.id2, hits(event.id2) + 1)
        current.put(event.id1, c1)
        current.put(event.id2, c2)
        current.keySet.foreach(i => if (i != event.id1) predict(event.id1, i, event.collisionTime))
        current.keySet.foreach(i => if (i != event.id2) predict(event.id2, i, event.collisionTime))
        lastCollisionTime = event.collisionTime
      }
    }
    current.transform((_, c) => collider.move(c, duration - lastCollisionTime))
    collidables.indices.map(i => current(i)).toVector
  }

  def predict(id1: Int, id2: Int, start: Double): Unit = {
    collider.collideTime(current(id1), current(id2)).foreach(t => pq.enqueue(buildEvent(id1, id2, t + start)))
  }

  def buildEvent(id1: Int, id2: Int, t: Double): CollisionEvent = {
    CollisionEvent(t, id1, hits.getOrElseUpdate(id1, 0), id2, hits.getOrElseUpdate(id2, 0))
  }

  def isEventValid(event: CollisionEvent): Boolean = {
    event.hits1 == hits(event.id1) && event.hits2 == hits(event.id2)
  }


}
