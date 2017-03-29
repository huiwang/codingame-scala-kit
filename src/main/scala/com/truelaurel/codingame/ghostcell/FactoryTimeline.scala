package com.truelaurel.codingame.ghostcell

case class FactoryState(id: Int, owner: Int, cyborgs: Int, balance: Int = 0)

object FactoryTimeline {

  def finalState(factory: Fac, troops: Vector[Troop], finalTurn: Int = GhostCellConstant.MAX_TURN): FactoryState = {
    var arrivals = troops
      .filter(_.to == factory.id)
      .sortBy(_.arrival)
    var owner = factory.owner
    var cyborgs = if(owner == -1) -factory.cyborgs else factory.cyborgs
    var balance = 0
    var turn = 0
    while (arrivals.nonEmpty && arrivals.head.arrival <= finalTurn) {
      val head = arrivals.head
      val (sameWithHead, nextArrivals) = arrivals.span(_.arrival == arrivals.head.arrival)
      val produced = owner * factory.production * (if(head.arrival <= factory.again) 0 else head.arrival - turn.max(factory.again))
      val arrivalResolved = sameWithHead.map(a => a.cyborgs * a.owner).sum
      balance = nextBalance(owner, balance, cyborgs, arrivalResolved)
      val after = afterResolving(owner, produced, cyborgs, arrivalResolved)
      owner = nextOwner(owner, produced, cyborgs, arrivalResolved, after)
      cyborgs = after
      turn = head.arrival
      arrivals = nextArrivals
    }
    if (turn < finalTurn) {
      FactoryState(factory.id, owner, cyborgs + owner * factory.production * (finalTurn - turn), balance)
    } else {
      FactoryState(factory.id, owner, cyborgs, balance)
    }
  }

  private def afterResolving(owner: Int, produced: Int, cyborgs: Int, arrivalResolved: Int) = {
    produced + (if (owner == 0) {
      if (arrivalResolved > 0) (arrivalResolved - cyborgs).abs else arrivalResolved + cyborgs
    } else arrivalResolved + cyborgs)
  }

  private def nextBalance(owner: Int, balance: Int, cyborgs: Int, arrivalResolved: Int) = {
    if (owner == 0) {
      balance + arrivalResolved.abs.min(cyborgs) * signum(arrivalResolved) * -1
    } else balance
  }


  private def nextOwner(owner: Int, produced: Int, cyborgs: Int, arrivalResolved: Int, after: Int) = {
    owner match {
      case 0 => if (arrivalResolved > cyborgs) 1 else if (arrivalResolved.abs > cyborgs) -1 else 0
      case _ => if (after == 0) {
        owner
      } else if (produced + cyborgs == 0) {
        signum(after)
      } else {
        owner * signum(after * (produced + cyborgs))
      }
    }
  }

  def signum(number: Int): Int = if (number == 0) 0 else if (number > 0) 1 else -1
}
