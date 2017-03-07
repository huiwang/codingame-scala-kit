package codingame.scala.ghostcell

object FactoryAnalysis {

  def movePlans(state: GhostCellGameState): Vector[MoveAction] = {
    val (sources, toLost) = state.factories.filter(mf => mf.mine).partition(mf => FactoryTimeline.finalState(mf, state.troops).owner == 1)
    val sourceBudget = sources.map(mf => mf.id -> available(mf, state)).toMap
    val targets = state.factories.filter(fac => !fac.mine) ++ toLost.filter(_.production > 0)
    val targetsSorted = targets.sortBy(target => evaluateFactory(target, sources, state, sourceBudget)).reverse
    val initPlan: Vector[MoveAction] = Vector.empty
    targetsSorted.foldLeft(initPlan) {
      case (totalMoves, sink) => {
        val sourcesSorted = sources.sortBy(src => state.dist(src.id, sink.id))
        planForConquer(sink, sourcesSorted, totalMoves, state, sourceBudget)
      }
    }
  }


  private def planForConquer(sink: Factory, sources: Vector[Factory], moves: Vector[MoveAction],
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

  private def evaluateFactory(sink: Factory, sources: Vector[Factory],
                              state: GhostCellGameState, availability: Map[Int, Int]) = {
    val moves = planForConquer(sink, sources, Vector.empty, state, availability)
    val troops = movesToTroops(moves, state)
    val finalState = FactoryTimeline.finalState(sink, troops ++ state.troops)
    finalState.cyborgs.toDouble / moves.map(_.cyborgs).sum
  }

  def movesToTroops(moves: Vector[MoveAction], state: GhostCellGameState): Vector[Troop] = {
    moves.map(m => Troop(id = Int.MaxValue, owner = state.factories(m.from).owner, m.from, m.to,
      m.cyborgs, state.dist(m.from, m.to) + 1))
  }

  private def allEnemiesAttack(target: Factory, state: GhostCellGameState): Vector[MoveAction] = {
    state.otherFacs.filter(otF => {
      otF.cyborgs > 0
    }).map(otF => MoveAction(otF.id, target.id, otF.cyborgs))
  }

  def available(src: Factory, state: GhostCellGameState): Int = {
    available(src, state, 0, src.cyborgs)
  }

  def available(src: Factory, state: GhostCellGameState, lower: Int, upper: Int): Int = {
    if (lower == upper) {
      src.cyborgs - lower
    } else {
      val middle = (lower + upper) / 2
      if (FactoryTimeline.finalState(src.copy(cyborgs = middle), state.troops).owner == 1) {
        available(src, state, lower, middle)
      } else {
        available(src, state, middle + 1, upper)
      }
    }
  }


  /**
    *
    * Move to be executed for the next round, then it takes the enough time(the distance) to reach the target
    * that's why we need to add increase the arrival time by one
    */
  def conquer(to: Factory, from: Factory, troops: Vector[Troop], state: GhostCellGameState): Int = {
    conquer(to, from, troops, state, 0, from.cyborgs)
  }

  def conquer(to: Factory, from: Factory, troops: Vector[Troop], state: GhostCellGameState,
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


}
