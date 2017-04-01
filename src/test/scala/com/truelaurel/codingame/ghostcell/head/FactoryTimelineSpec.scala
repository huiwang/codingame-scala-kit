package com.truelaurel.codingame.ghostcell.head

import com.truelaurel.codingame.ghostcell.common.{Fac, FactoryState, GhostCellConstant, Troop}
import com.truelaurel.codingame.ghostcell.common.FactoryTimeline.finalState
import org.scalatest.{FlatSpec, Matchers}

class FactoryTimelineSpec extends FlatSpec with Matchers {

  behavior of "FactoryTimelineSpec"

  it should "final state should not be changed without any troop" in {
    finalState(Fac(1, 1, 0, 0, 0), Vector.empty) should be(FactoryState(1, 1, 0))
  }

  it should "final state should change owner without production " in {
    finalState(Fac(1, 1, 0, 0, 0),
      Vector(
        Troop(id = 2, owner = -1, from = 8, to = 1, cyborgs = 2, arrival = 20)
      )
    ) should be(FactoryState(1, -1, -2))
  }

  it should "final state should have more cyborgs with production" in {
    finalState(Fac(1, 1, 0, 1, 0), Vector.empty) should be(FactoryState(1, 1, GhostCellConstant.MAX_TURN))
  }

  it should "final state should change owner" in {
    finalState(Fac(id = 1, owner = 1, cyborgs = 0, production = 1, again = 0),
      Vector(
        Troop(id = 2, owner = -1, from = 8, to = 1, cyborgs = 2, arrival = 1)
      )
    ) should be(FactoryState(1, -1, -GhostCellConstant.MAX_TURN))
  }

  it should "final state should change owner with oppo target" in {
    finalState(Fac(id = 2, owner = -1, cyborgs = 2, production = 1, again = 0),
      Vector(
        Troop(id = 2, owner = 1, from = 8, to = 2, cyborgs = 4, arrival = 1)
      )
    ) should be(FactoryState(2, 1, GhostCellConstant.MAX_TURN))
  }

  it should "final state should handle multiple troops arriving at different time" in {
    finalState(Fac(id = 1, owner = 1, cyborgs = 0, production = 1, again = 0),
      Vector(
        Troop(id = 2, owner = 1, from = 8, to = 1, cyborgs = 1, arrival = 2),
        Troop(id = 3, owner = -1, from = 9, to = 1, cyborgs = 4, arrival = 2),
        Troop(id = 4, owner = 1, from = 10, to = 1, cyborgs = 7, arrival = 4)
      )
    ) should be(FactoryState(1, 1, GhostCellConstant.MAX_TURN))
  }

  it should "final state should handle balance" in {
    finalState(Fac(id = 1, owner = 0, cyborgs = 10, production = 1, again = 0),
      Vector(
        Troop(id = 2, owner = 1, from = 8, to = 1, cyborgs = 9, arrival = 1),
        Troop(id = 3, owner = -1, from = 9, to = 1, cyborgs = 3, arrival = 2)
      )
    ) should be(FactoryState(1, -1, -GhostCellConstant.MAX_TURN, -8))
  }

  it should "final state should handle bomb" in {
    finalState(Fac(id = 1, owner = 1, cyborgs = 1, production = 1, again = 1),
      Vector(
        Troop(id = 2, owner = -1, from = 8, to = 1, cyborgs = 2, arrival = 1)
      )
    ) should be(FactoryState(1, -1, -GhostCellConstant.MAX_TURN))

  }

}
