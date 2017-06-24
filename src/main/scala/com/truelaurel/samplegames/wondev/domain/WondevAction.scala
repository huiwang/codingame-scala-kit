package com.truelaurel.samplegames.wondev.domain

sealed trait Direction

case object N extends Direction

case object W extends Direction

case object S extends Direction

case object E extends Direction

case object NW extends Direction

case object NE extends Direction

case object SW extends Direction

case object SE extends Direction

object Direction {

  val all = Vector(N, W, S, E, SW, SE, NW, NE)

  def apply(dir: String): Direction = dir match {
    case "N" => N
    case "S" => S
    case "W" => W
    case "E" => E
    case "NE" => NE
    case "SE" => SE
    case "NW" => NW
    case "SW" => SW
    case _ => throw new IllegalArgumentException("unknown direction " + dir)
  }
}

sealed trait WondevAction

sealed case class MoveBuild(unitIndex: Int, moveDir: Direction, buildDir: Direction) extends WondevAction {
  override def toString: String = s"MOVE&BUILD $unitIndex $moveDir $buildDir"
}

sealed case class MovePush(unitIndex: Int, moveDir: Direction, pushDir: Direction) extends WondevAction {
  override def toString: String = s"PUSH&BUILD $unitIndex $moveDir $pushDir"
}
