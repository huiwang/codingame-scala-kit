package codingame.scala.kit.ghostcell

object FactoryAnalysis {

  def movePlans(state: GhostCellGameState): Vector[MoveAction] = {
    val (sources, toLost) = state.myFacs.partition(
      mf => FactoryTimeline.finalState(mf, state.troops).owner == 1)
    val sourceBudget = sources.map(mf => mf.id -> moveAvailable(mf, state)).toMap
    val increasable = increaseableSources(state)
    val conquerable = state.factories.filter(fac => !fac.mine) ++ toLost.filter(_.production > 0)
    val conquerableToScoreRaw = conquerable.map(target => (target, evaluateFactoryConquer(target, sources, state, sourceBudget)))
    val increasableToScoreRaw = increasable.map(target => (target, evaluateFactoryInc(target, sources.filter(_.id != target.id), state, sourceBudget)))
    val targetToScoreSorted = (conquerableToScoreRaw ++ increasableToScoreRaw).sortBy(_._2).reverse
    val targetsSorted = targetToScoreSorted.map(_._1)
    val initPlan: Vector[MoveAction] = Vector.empty
    targetsSorted.foldLeft(initPlan) {
      case (totalMoves, sink) => {
        val sourcesSorted = sources.filter(_.id != sink.id).sortBy(src => state.dist(src.id, sink.id))
        if (conquerable.contains(sink)) {
          planForConquer(sink, sourcesSorted, totalMoves, state, sourceBudget)
        } else if (increasable.contains(sink)) {
          planForInc(sink, sourcesSorted, totalMoves, state, sourceBudget) :+ MoveAction(sink.id, sink.id, sink.cyborgs)
        } else {
          totalMoves
        }
      }
    }.filter(m => m.from != m.to)
  }

  def noIncrease(state: GhostCellGameState): Boolean = state.bombs.count(_.owner == -1) > 0 || state.center.owner == 0


  def increaseableSources(state: GhostCellGameState): Vector[Fac] = {
    if (noIncrease(state)) Vector.empty else {
      state.myFacs.filter(mf => FactoryTimeline.finalState(mf, state.troops).owner == 1)
        .filter(fac => fac.production < 3).filter(fac => !mayExplode(fac, state))
    }
  }

  def mayExplode(fac: Fac, state: GhostCellGameState): Boolean = {
    state.bombs
      .filter(_.owner == -1)
      .exists(b => {
        val dist = state.directDist(b.from, fac.id)
        val travelled = state.turn - b.birth
        val remaining = dist - travelled
        remaining == 1
      })
  }

  private def planForConquer(sink: Fac, sources: Vector[Fac], moves: Vector[MoveAction],
                             state: GhostCellGameState, availability: Map[Int, Int]): Vector[MoveAction] = {
    if (sources.isEmpty) moves else {
      val src = sources.head
      val used = moves.filter(_.from == src.id).map(_.cyborgs).sum
      val available = availability(src.id) - used
      if (available <= 0) {
        planForConquer(sink, sources.tail, moves, state, availability)
      }
      else {
        val move = conquer(sink, src, state.troops ++ movesToTroops(moves, state), state, 0, available)
        if (move == 0) {
          moves
        } else {
          planForConquer(sink, sources.tail, moves :+ MoveAction(src.id, sink.id, move), state, availability)
        }
      }
    }
  }

  private def evaluateFactoryConquer(sink: Fac, sources: Vector[Fac],
                                     state: GhostCellGameState, availability: Map[Int, Int]) = {
    val moves = planForConquer(sink, sources, Vector.empty, state, availability)
    val investments = moves.map(m => m.cyborgs * Math.pow(2, state.dist(m.from, m.to))).sum
    state.facValue(sink.id) / (1.0 + investments)
  }


  private def evaluateFactoryInc(sink: Fac, sources: Vector[Fac],
                                 state: GhostCellGameState, availability: Map[Int, Int]) = {
    val moves = planForInc(sink, sources, Vector.empty, state, availability)
    val investments = (moves :+ MoveAction(sink.id, sink.id, sink.cyborgs)).map(m => m.cyborgs * Math.pow(2, state.dist(m.from, m.to))).sum
    state.facValue(sink.id) / (1.0 + investments)
  }

  def movesToTroops(moves: Vector[MoveAction], state: GhostCellGameState): Vector[Troop] = {
    moves.map(m => Troop(id = Int.MaxValue, owner = state.factories(m.from).owner, m.from, m.to,
      m.cyborgs, state.dist(m.from, m.to) + 1))
  }

  def incAvailable(src: Fac, state: GhostCellGameState): Int = {
    val neighborMoves = state.otherFacs
      .filter(fac => state.dist(fac.id, src.id) <= 3)
      .map(fac => MoveAction(fac.id, src.id, fac.cyborgs))

    available(src, state.troops ++ movesToTroops(neighborMoves, state), 0, src.cyborgs)
  }

  def moveAvailable(src: Fac, state: GhostCellGameState): Int = {
    available(src, generateTroopWithNeighborMoves(src, state, 1), 0, src.cyborgs)
  }

  private def generateTroopWithNeighborMoves(src: Fac, state: GhostCellGameState, dist: Int): Vector[Troop] = {
    state.troops ++ movesToTroops(generateNeighborMoves(src, state, dist), state)
  }

  private def generateNeighborMoves(src: Fac, state: GhostCellGameState, dist: Int): Vector[MoveAction] = {
    state.otherFacs
      .filter(fac => fac.production < src.production && state.dist(fac.id, src.id) <= dist)
      .map(fac => MoveAction(fac.id, src.id, fac.cyborgs))
  }

  def available(src: Fac, troops: Vector[Troop], lower: Int, upper: Int): Int = {
    if (lower == upper) {
      src.cyborgs - lower
    } else {
      val middle = (lower + upper) / 2
      if (FactoryTimeline.finalState(src.copy(cyborgs = middle), troops).owner == 1) {
        available(src, troops, lower, middle)
      } else {
        available(src, troops, middle + 1, upper)
      }
    }
  }


  /**
    *
    * Move to be executed for the next round, then it takes the enough time(the distance) to reach the target
    * that's why we need to add increase the arrival time by one
    */
  def conquer(to: Fac, from: Fac, troops: Vector[Troop], state: GhostCellGameState): Int = {
    conquer(to, from, troops, state, 0, from.cyborgs)
  }

  def conquer(to: Fac, from: Fac, troops: Vector[Troop], state: GhostCellGameState,
              lower: Int, upper: Int): Int = {
    if (lower == upper) {
      lower
    } else {
      val middle = (lower + upper) / 2
      val troop = Troop(id = Int.MaxValue,
        owner = from.owner,
        from = from.id,
        to = to.id,
        cyborgs = middle,
        arrival = state.dist(from.id, to.id) + 1)
      if (FactoryTimeline.finalState(to, troops :+ troop).owner == 1) {
        conquer(to, from, troops, state, lower, middle)
      } else {
        conquer(to, from, troops, state, middle + 1, upper)
      }
    }

  }

  private def planForInc(sink: Fac, sources: Vector[Fac], moves: Vector[MoveAction],
                         state: GhostCellGameState, availability: Map[Int, Int]): Vector[MoveAction] = {
    if (sources.isEmpty) moves else {
      val src = sources.head
      val used = moves.filter(_.from == src.id).map(_.cyborgs).sum
      val available = availability(src.id) - used
      if (available <= 0) {
        planForInc(sink, sources.tail, moves, state, availability)
      }
      else {
        val move = inc(sink, src, state.troops ++ movesToTroops(moves, state), state, 0, available)
        if (move == 0) {
          moves
        } else {
          planForInc(sink, sources.tail, moves :+ MoveAction(src.id, sink.id, move), state, availability)
        }
      }
    }
  }

  def inc(to: Fac, from: Fac, troops: Vector[Troop], state: GhostCellGameState): Int = {
    inc(to, from, troops, state, 0, from.cyborgs)
  }

  def inc(to: Fac, from: Fac, troops: Vector[Troop], state: GhostCellGameState,
          lower: Int, upper: Int): Int = {
    if (lower == upper) {
      lower
    } else {
      val middle = (lower + upper) / 2
      val troop = Troop(id = Int.MaxValue,
        owner = from.owner,
        from = from.id,
        to = to.id,
        cyborgs = middle,
        arrival = state.dist(from.id, to.id) + 1)
      val finalState = FactoryTimeline.finalState(to, troops :+ troop, troop.arrival)
      if (finalState.owner == 1 && finalState.cyborgs >= 10) {
        inc(to, from, troops, state, lower, middle)
      } else {
        inc(to, from, troops, state, middle + 1, upper)
      }
    }

  }


}
