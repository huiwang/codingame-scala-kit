package com.truelaurel.samplegames.wondev.analysis

import com.truelaurel.math.geometry.Pos
import com.truelaurel.samplegames.wondev.domain._

/**
  * Created by hwang on 24/06/2017.
  */
object WondevAnalysis {

  def neighborIn(pos: Pos, direction: Direction): Pos = direction match {
    case N => pos.copy(y = pos.y - 1)
    case S => pos.copy(y = pos.y + 1)
    case W => pos.copy(x = pos.x - 1)
    case E => pos.copy(x = pos.x + 1)
    case NE => pos.copy(x = pos.x + 1, y = pos.y - 1)
    case SE => pos.copy(x = pos.x + 1, y = pos.y + 1)
    case NW => pos.copy(x = pos.x - 1, y = pos.y - 1)
    case SW => pos.copy(x = pos.x - 1, y = pos.y + 1)
  }

  def neighborsOf(pos: Pos, size: Int): Seq[Pos] = {
    Direction.all.map(neighborIn(pos, _)).filter(p => p.x < size && p.x > 0 && p.y < size && p.y > 0)
  }

  def evaluate(state: WondevState): Double = {
    ???
  }

  def heightOf(pos: Pos, state: WondevState): Int = {
    state.heights(pos)
  }
}
