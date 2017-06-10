package ai.scala.fp.geo

case class Pos(x: Int, y: Int) {
  def neighbours4: Seq[Pos] =
    Seq(Pos(x + 1, y),
      Pos(x - 1, y),
      Pos(x, y - 1),
      Pos(x, y + 1))
}

object Pos {
  val right = (1, 0)
  val down = (0, 1)
  val downRight = (1, 1)
  val downLeft = (-1, 1)
  val all = Seq(right, down, downRight, downLeft)
}
