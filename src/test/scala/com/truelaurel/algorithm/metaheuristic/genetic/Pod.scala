package com.truelaurel.algorithm.metaheuristic.genetic

case class Vec(speed: Int = 0, rotate: Int = 0)

case class MoveSet(vecarr: Array[Vec]) {}

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
    (x, y)
  }
}
