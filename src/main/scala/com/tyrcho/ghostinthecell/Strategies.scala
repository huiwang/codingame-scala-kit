package com.tyrcho.ghostinthecell

import ai.scala.fp.State
import CodinGameTemplate._

object Strategies {
  type Strategy = State[GameState, Vector[Move]]

  val KeepDefenses: Strategy = State { initial =>
    val moves = initial.movesKeepDefenses
    debug("keepDefenders:" + moves)
    val remaining = initial.applyOrders(moves)
    (remaining, moves)
  }

  val StayToUpgrade: Strategy = State { initial =>
    val moves = initial.movesStayToUpgrade
    debug("Stay to Upgrade:" + moves)
    val remaining = initial.applyOrders(moves)
    (remaining, moves)
  }

  val Conquer: Strategy = State { initial =>
    //    debug("conquer : initial" + initial.troops)
    val moves = initial.movesConquer
    debug("conquer:" + moves)
    val remaining = initial.applyOrders(moves)
    (remaining, moves)
  }

  val Center: Strategy = State { initial =>
    val moves = initial.myFactories.flatMap(initial.centerMove)
    val remaining = initial.applyOrders(moves)
    debug("center:" + moves)
    (remaining, moves)
  }

  val AvoidBombs: Strategy = State { initial =>
    val moves = initial.myFactories
      .filter(f => initial.factoriesWhichCanReceiveBombNow.contains(f.id))
      .map(initial.avoidMove)
    val remaining = initial.applyOrders(moves)
    //    debug("avoid:" + moves)
    (remaining, moves)
  }

  val Upgrade: Strategy = State { initial =>
    val moves = initial.movesUpgrade
    val remaining = initial.applyOrders(moves)
    debug("upgrade:" + moves)
    (remaining, moves)
  }

  val SendBomb: Strategy = State { initial =>
    val moves = initial.movesBomb
    val remaining = initial.applyOrders(moves)
    //    debug("send bomb:" + moves)
    (remaining, moves)
  }

}