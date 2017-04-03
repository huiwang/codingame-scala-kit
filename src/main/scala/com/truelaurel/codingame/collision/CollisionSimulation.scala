package com.truelaurel.codingame.collision

import scala.collection.mutable

trait Collidable {
  def id: Int
}

trait CollisionResolver[T] {

  def collideTime(c1: T, c2: T): Option[Double]

  def bounceOff(c1: T, c2: T): (T, T)

  def move(collidable: T, time : Double) : T
}

case class CollisionEvent(collisionTime: Double, id1: Int, hits1: Int, id2: Int, hits2: Int) extends Ordered[CollisionEvent] {
  override def compare(that: CollisionEvent): Int = that.collisionTime.compare(collisionTime)
}

class CollisionSimulation[T <: Collidable](val resolver: CollisionResolver[T], val duration: Double = 1.0) {

  private val pq = new mutable.PriorityQueue[CollisionEvent]
  private val hits = new mutable.HashMap[Int, Int]()
  private val current = new mutable.HashMap[Int, T]()

  def simulate(collidables: List[T]): List[T] = {
    collidables.combinations(2).foreach {
      case c1 :: c2 :: Nil => predict(c1, c2, 0)
      case _ =>
    }
    collidables.foreach(c => current.put(c.id, c))
    var lastCollisionTime = 0.0
    while (pq.nonEmpty) {
      val event = pq.dequeue()
      if (isEventValid(event) && event.collisionTime < duration) {
        current.transform((_, c) => resolver.move(c, event.collisionTime - lastCollisionTime))
        val (c1, c2) = resolver.bounceOff(current(event.id1), current(event.id2))
        hits.put(c1.id, hits(c1.id) + 1)
        hits.put(c2.id, hits(c2.id) + 1)
        current.put(c1.id, c1)
        current.put(c2.id, c2)
        current.values.withFilter(_.id != c1.id).foreach(c => predict(c1, c, event.collisionTime))
        current.values.withFilter(_.id != c2.id).foreach(c => predict(c2, c, event.collisionTime))
        lastCollisionTime = event.collisionTime
      }
    }
    current.transform((_, c) => resolver.move(c, duration - lastCollisionTime))
    collidables.map(c => current(c.id))
  }

  def predict(c1: T, c2: T, start: Double): Unit = {
    resolver.collideTime(c1, c2).foreach(t => pq.enqueue(buildEvent(c1, c2, t + start)))
  }

  def buildEvent(c1: Collidable, c2: Collidable, t: Double): CollisionEvent = {
    CollisionEvent(t, c1.id, hits.getOrElseUpdate(c1.id, 0), c2.id, hits.getOrElseUpdate(c2.id, 0))
  }

  def isEventValid(event: CollisionEvent): Boolean = {
    event.hits1 == hits(event.id1) && event.hits2 == hits(event.id2)
  }


}
