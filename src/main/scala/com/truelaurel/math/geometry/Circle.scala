package com.truelaurel.math.geometry

import scala.math.sqrt

case class Circle(center: Pos, radius: Int) {
  def sqr(x: Double) = x * x

  // http://nains-games.over-blog.com/2014/12/intersection-de-deux-cercles.html
  // https://pastebin.com/5hT3G3V8
  def intersections(ci: Circle): Seq[Pos] = {
    val rc1 = radius
    val rc2 = ci.radius
    val xc1 = center.x
    val yc1 = center.y
    val xc2 = ci.center.x
    val yc2 = ci.center.y

    if (yc1 == yc2) {
      val a = (xc1 - xc2).abs
      val XIa = (sqr(rc2) - sqr(a) - sqr(rc1)) / (-2 * a)
      val root = sqr(rc2) - sqr(a - XIa)
      if (root >= 0) {
        val YIa = yc1 + sqrt(root)
        val YIb = yc1 - sqrt(root)
        Seq(new Pos(XIa, YIa), new Pos(XIa, YIb)).distinct
      } else Seq.empty
    } else {
      val a = (-sqr(xc1) - sqr(yc1) + sqr(xc2) + sqr(yc2) + sqr(rc1) - sqr(
        rc2
      )) / (2 * (yc2 - yc1))
      val d = (xc2.toDouble - xc1) / (yc2 - yc1)
      val A = sqr(d) + 1
      val B = -2 * xc1 + 2 * yc1 * d - 2 * a * d
      val C = sqr(xc1) + sqr(yc1) - 2 * yc1 * a + sqr(a) - sqr(rc1)
      val delta = sqr(B) - 4 * A * C
      if (delta >= 0) {
        val XIa = (-B + sqrt(delta)) / (2 * A)
        val XIb = (-B - sqrt(delta)) / (2 * A)
        val YIa = a - ((-B + sqrt(delta)) / (2 * A)) * d
        val YIb = a - ((-B - sqrt(delta)) / (2 * A)) * d
        Seq(new Pos(XIa, YIa), new Pos(XIb, YIb)).distinct
      } else Seq.empty
    }
  }
}
