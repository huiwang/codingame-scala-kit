package com.truelaurel.algorithm.metaheuristic.genetic

import com.truelaurel.time.CountStopper

import org.scalatest.{FlatSpec, Matchers}

class Vec(val speed: Int = 0, val rotate: Int = 0) {}
class MoveSet(val vecarr: Array[Vec]) {}
class Pod {
  var x: Int = 0
  var y: Int = 0
  var speedx: Int = 0
  var speedy: Int = 0
  var rotate: Int = 0
  var count: Int = 0
  def move(v: Vec): (Int, Int) = {
    rotate += Math.min(18, Math.max(-18, v.rotate - rotate))
    val tmpspeed = Math.min(200, Math.max(0, v.speed))
    val tmpag = rotate * Math.PI / 180
    val tmpspeedx = Math.cos(tmpag) * tmpspeed
    val tmpspeedy = Math.sin(tmpag) * tmpspeed
    speedx = (speedx * 0.85 + tmpspeedx).toInt
    speedy = (speedy * 0.85 + tmpspeedy).toInt
    x += speedx
    y += speedy
    return (x, y)
  }
}
class WayPoint(val x: Int, val y: Int) {
  var next: WayPoint = null
  def dist(p: Pod): Double = Math.sqrt(Math.pow(x - p.x, 2) + Math.pow(y - p.y, 2))
  def touch(p: Pod): Boolean = dist(p) < 400
}

class Referee(val wps: Array[WayPoint] = null) {
  val wpsize = wps.size
  def evaluate(ms: MoveSet): Double = {
    val pod = new Pod
    var i = 1
    for (v <- ms.vecarr) {
      val (x, y) = pod.move(v)
      if (wps(i % wpsize).touch(pod)) {
        i += 1
      }
    }
    return i * 1000 - wps(i % wpsize).dist(pod)
  }
}

class MSGR(val ref: Referee = null) extends GeneticRepresentation[MoveSet] {
  override def mutate(solution: MoveSet): MoveSet = {
    for (i <- 0 until 20) {
      if (scala.util.Random.nextInt.abs % 100 < 10)
        solution.vecarr(i) = new Vec(scala.util.Random.nextInt.abs % 200, scala.util.Random.nextInt % 180)
    }
    return solution
  }
  override def randomSolution: MoveSet = {
    val vecarr = new Array[Vec](20)
    for (i <- 0 until 20)
      vecarr(i) = new Vec(scala.util.Random.nextInt.abs % 200, scala.util.Random.nextInt % 180)
    return new MoveSet(vecarr)
  }
  override def crossover(solutionA: MoveSet, solutionB: MoveSet): (MoveSet, MoveSet) = {
    val vecarrA = new Array[Vec](20)
    val vecarrB = new Array[Vec](20)
    for (i <- 0 until 20) {
      val vA = solutionA.vecarr(i)
      val sA = vA.speed
      val rA = vA.rotate
      val vB = solutionB.vecarr(i)
      val sB = vB.speed
      val rB = vB.rotate
      val nsA = (sA + scala.util.Random.nextDouble * (sB - sA)).toInt
      val nrA = (rA + scala.util.Random.nextDouble * (rB - rA)).toInt
      vecarrA(i) = new Vec(nsA, nrA)
      val nsB = (sA + scala.util.Random.nextDouble * (sB - sA)).toInt
      val nrB = (rA + scala.util.Random.nextDouble * (rB - rA)).toInt
      vecarrB(i) = new Vec(nsB, nrB)
    }
    return (new MoveSet(vecarrA), new MoveSet(vecarrB))
  }
  override def assess(solution: MoveSet): AssessedSolution[MoveSet] =
    new AssessedSolution[MoveSet](solution, ref.evaluate(solution))
}

class GeneticAlgorithmTest extends FlatSpec with Matchers {

  behavior of "GeneticAlgorithm"

  val wps: Array[WayPoint] = Array(
    new WayPoint(0, 0),
    new WayPoint(1000, 0)
  )
  val ref = new Referee(wps)
  val msgr = new MSGR(ref)

  val popSize = 100
  val tournamentSize = 20
  val eliteSize = 20
  val stopper = new CountStopper(1000)

  val ga = new GeneticAlgorithm[MoveSet](popSize, tournamentSize, eliteSize, stopper)

  it can "search for a good result" in {
    val result = ga.search(msgr)
    ref.evaluate(result) should be >= 2000.0
  }

}
