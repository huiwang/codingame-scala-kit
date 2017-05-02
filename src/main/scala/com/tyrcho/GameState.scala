package com.tyrcho

import CodinGameTemplate._
import ai.scala.fp.IterableUtil._

import scala.util.Random
import Strategies._
import ai.scala.fp.State

import scala.collection.Seq
import scala.collection.immutable.Stream.consWrapper

case class GameState(
    grid: Grid,
    troops: Vector[Troop] = Vector(),
    factories: Vector[Factory] = Vector(),
    bombs: Vector[Bomb] = Vector(),
    future: Int = 0,
    enBombsSent: Set[Int] = Set(),
    myBombsSent: Set[Int] = Set()) {

  def distance(f1: Int, f2: Int) = grid.distance(f1, f2)

  val myTroops = troops.filter(_.mine).sortBy(_.timeLeft)
  val enTroops = troops.filterNot(_.mine).sortBy(_.timeLeft)
  val myFactories = factories.filter(_.mine == Some(true)).sortBy(-_.production)
  val enFactories = factories.filter(_.mine == Some(false)).sortBy(-_.production)
  val neutralFactories = factories.filter(_.mine == None).sortBy(-_.production)
  val possibleTargets = (enFactories ++ neutralFactories).filter(_.production > 0)

  def nextMoves: Vector[Move] = {
    State.sequence(Seq(KeepDefenses) ++ Seq.fill(8)(Conquer) ++ Seq(SendBomb, AvoidBombs, StayToUpgrade, Upgrade, Center)).eval(this).flatten
  }

  def movesUpgrade: Vector[IncMove] = {
    def stayedInFact(id: Int) = troops.collect {
      case t if t.isStayMove => t.cyborgs
    }.sum

    myFactories
      .filterNot(f =>
        //        enBombsSent.size != 2 ||
        f.maxProduction == 3 ||
          stayedInFact(f.id) + f.cyborgs < 10 ||
          //          initial.canCapture(false, f.id, 10) ||
          factoriesWhichCanReceiveBombAnytime.contains(f.id))
      .map(f => IncMove(f.id))
  }

  def movesStayToUpgrade: Vector[TroopMove] = for {
    f <- myFactories
    if f.cyborgs > 0
    id = f.id
    if f.production < 3
  } yield TroopMove(id, id, 10 min f.cyborgs)

  def movesConquer = bestConquestPlan.toVector.flatMap { p =>
    //    debug("factory before conquest " + nextStates(p.achieved).factories(p.plans.head.to))
    p.plans.collect {
      case ConquestPlan(from, to, cyborgs, delay) if iCanSendFrom(from) =>
        TroopMove(from, targetWithStop(from, to), cyborgs min factories(from).cyborgs, Some(to))
    }
  }

  def movesKeepDefenses: Vector[TroopMove] = for {
    f <- myFactories
    if f.cyborgs > 0
    id = f.id
    if f.maxProduction > 0
    delta = (0 to 5).map(incomingDeltaUntilTurn(f)).takeWhile(_ <= f.cyborgs).max // defend as long as possible
    if delta > 0
  } yield TroopMove(id, id, delta min f.cyborgs)

  def incomingDeltaUntilTurn(factory: Factory)(turn: Int): Int =
    if (turn == 0) 0
    else {
      val id = factory.id
      troops.collect {
        case Troop(_, mine, _, `id`, cyborgs, t, _) if t <= turn =>
          if (mine) -cyborgs else cyborgs
      }.sum - factory.production * turn
    }

  def bestConquestPlan = {
    val plans = for {
      f <- troopsArrived.enFactories ++ troopsArrived.neutralFactories
      if !hasConquestMove(f.id)
      conquestPlan = earliestConquest(f.id, myFactories)
      if !conquestPlan.failed //&& !conquestPlan.delayed
      s = score(conquestPlan)
      if s > 0
    } yield s -> conquestPlan
    debug("all conquest plans " + plans)
    val best = plans.maxOption(_._1).map(_._2)
    debug("best conquest plan " + best)
    best
  }

  def iCanSendFrom(from: Int) = {
    val f = factories(from)
    f.cyborgs > 0 && f.mine == Some(true)
  }

  def score(plans: ConquestPlans) = {
    val cost = plans.achieved * plans.cyborgs
    val captured = factories(plans.plans.head.to)
    val benefit =
      if (captured.mine == None)
        captured.maxProduction * (20 - plans.achieved) - plans.cyborgs
      else
        2 * captured.maxProduction * (20 - plans.achieved)
    benefit * 100 / cost
  }

  def movesBomb: Vector[BombMove] = {
    def bombScore(target: Factory) =
      if (target.production >= 2)
        Some(target.cyborgs / 2 max 10 + target.production * 5)
      else None

    val possibleMoves = for {
      from <- myFactories
      if canSendBomb
      to <- factories
      if to != from
      dist = distance(from.id, to.id)
      state = nextStates(dist)
      if state.nextStates.take(5).forall(_.factories(to.id).disabled <= 0) // avoid sending a bomb which arrives earlier
      realTo = state.factories(to.id)
      owner <- realTo.mine
      if !owner
      score <- bombScore(realTo)
      //      _ = debug(realTo)
    } yield BombMove(from.id, to.id) -> (score - dist)
    possibleMoves.uniqueBy(_._1.to).sortBy(-_._2).map(_._1).take(2)
  }

  def canSendBomb = myBombsSent.size < 2

  def canSendTo(target: Int, from: Int, turns: Int): Int = {
    val timeLeft = turns - distance(from, target)
    if (timeLeft < 0) 0
    else {
      factories(from).cyborgs + timeLeft * factories(from).production
    }
  }

  def avoidMove(fa: Factory) =
    TroopMove(fa.id, factories
      .filter(f => f.id != fa.id && (f.mine != None || f.cyborgs == 0))
      .minBy(f => distance(f.id, fa.id)).id, fa.cyborgs)

  def centerMove(myFactory: Factory): Option[TroopMove] = {
    val id = myFactory.id
    myFactory.canSendToCenter(0).map { count =>
      TroopMove(myFactory.id, targetWithStop(myFactory.id, 0), count, Some(0))
    }
  }

  def targetWithStop(from: Int, to: Int) = {
    grid
      .stops(from, to)
      .filter(s => factories(s).mine.nonEmpty || factories(s).cyborgs == 0)
      .minOption(s => grid.distance(from, s))
      .getOrElse(to)
  }

  //Lower is better
  def factoryScore(from: Factory)(target: Factory) =
    -target.production *
      (if (target.mine == None) 100 else 200) /
      (target.cyborgs + distance(target.id, from.id))

  def debugState() = {
    //    debug(grid)
    //    bombs.foreach(debug)
    //    factoriesWhichCanReceiveBombNow.foreach(f => debug(s"$f can be hit by a bomb next turn"))
    def totalProduction(fs: Iterable[Factory]) = fs.map(_.production).sum
    debug(s"MINE (${totalProduction(myFactories)})")
    //    myFactories.foreach(debug)
    debug(s"OPP (${totalProduction(enFactories)})")
    //    enFactories.foreach(debug)
    debug(s"NEUTRAL (${totalProduction(neutralFactories)})")
    //    neutralFactories.foreach(debug)
    //    myTroops.foreach(debug)
    //    enTroops.foreach(debug)
  }

  def attackersNeeded(target: Int) = {
    val f = factories(target)
    if (f.mine.isEmpty) f.cyborgs
    else f.cyborgs + f.production
  }

  def earliestConquest(target: Int, avail: Vector[Factory] = myFactories): ConquestPlans = {
    val available = avail.filterNot(_.id == target)
    if (available.isEmpty) ConquestPlans(Vector(), 0)
    else {
      val maybeTurn = (for {
        turn <- (1 to 10).toStream
        totalSent = available.map(from => canSendTo(target, from.id, turn)).sum
        defenders = nextStates(turn).attackersNeeded(target)
        if (totalSent > defenders)
      } yield turn).headOption
      maybeTurn match {
        case Some(turn) =>
          val defenders = nextStates(turn).attackersNeeded(target)
          val (conquestPlans, rest) = available.sortBy(f => distance(f.id, target)).foldLeft((Vector.empty[ConquestPlan], defenders + 1)) {
            case ((plans, cyborgs), from) =>
              val send = canSendTo(target, from.id, turn) min cyborgs
              if (send <= 0) (plans, cyborgs)
              else {
                val plan = ConquestPlan(from = from.id, target, send, turn - distance(from.id, target))
                (plans :+ plan, cyborgs - send)
              }
          }
          assert(rest == 0)
          ConquestPlans(conquestPlans, turn)
        case _ => ConquestPlans(Vector(), 0)
      }
    }
  }

  // head is this
  lazy val nextStates: Stream[GameState] = (this #:: nextStates.map(_.nextState))

  lazy val troopsArrived: GameState = nextStates.find(s => s.troops.isEmpty && s.bombs.forall(_.timeLeft < 0)).get

  lazy val statesUntilStable = nextStates.takeWhile(_ != troopsArrived)

  def nextState = {
    val nextTroops = troops.map { t =>
      t.copy(timeLeft = t.timeLeft - 1)
    }
    val nextBombs = bombs.map { b =>
      b.nextTurn
    }
    val (arrived, ongoing) = nextTroops.partition(_.timeLeft == 0)
    val (arrivedBombs, ongoingBombs) = nextBombs.partition(_.timeLeft == 0)

    val arrivedAdded = arrived.foldLeft(Map.empty[Int, Int].withDefaultValue(0)) {
      case (map, troop) => map.updated(troop.to, map(troop.to) + troop.cyborgs * (if (troop.mine) 1 else -1))
    }

    val nextFactories =
      factories
        .map { f =>
          f.mine match {
            case None => f
            case _    => f.copy(cyborgs = f.cyborgs + f.production)
          }
        }.map { f =>
          f.receiveCyborgs(arrivedAdded(f.id))
            .nextTurn
        }.map { f =>
          if (arrivedBombs.exists(_.to == f.id))
            f.receiveBomb
          else f
        }

    copy(factories = nextFactories, troops = ongoing, future = future + 1, bombs = ongoingBombs)
  }

  def applyOrders(moves: Vector[Move]): GameState = {
    moves.foldLeft(this) {
      case (s, m) => s.applyMove(m)
    }
  }

  def applyMove(m: Move) = m match {
    case m: TroopMove => applyTroop(m)
    case m: IncMove   => applyInc(m)
    case m: BombMove  => applyBomb(m)
  }

  def applyTroop(move: TroopMove): GameState = {
    val from = factories(move.from)
    val to = factories(move.to)
    val from2 = from.copy(cyborgs = from.cyborgs - move.cyborgs)
    assert(from2.cyborgs >= 0)
    val fa = factories.updated(factories.indexOf(from), from2)
    val id = troops.maxOption(_.id).map(_.id).getOrElse(-1) + 1
    val owner = from.mine match {
      case Some(o) => o
      case None    => throw new Exception(move.toString)
    }
    val troop = Troop(id, mine = owner,
      from = move.from, to = move.to,
      cyborgs = move.cyborgs,
      timeLeft = distance(move.from, move.to) + 1,
      target = move.target)
    copy(factories = fa, troops = troops :+ troop)
  }

  def hasConquestMove(target: Int) =
    troops.exists(t => t.target == Some(target) && t.timeLeft > distance(t.from, t.to))

  def applyInc(move: IncMove): GameState = {
    val from = factories(move.factory)
    val from2 = from.upgrade
    copy(factories = factories.updated(factories.indexOf(from), from2))
  }

  def applyBomb(move: BombMove): GameState = {
    val owner = factories(move.from).mine match {
      case Some(o) => o
      case None    => throw new Exception(move.toString)
    }
    val bomb = Bomb(
      id = Random.nextInt(),
      mine = owner,
      from = move.from,
      to = move.to,
      timeLeft = distance(move.from, move.to),
      time = 0)

    copy(bombs = bombs :+ bomb, myBombsSent = myBombsSent + 1)
  }

  def nextState(moves: Vector[Move]): GameState = applyOrders(moves).nextState

  def updateBombs(bombs: Vector[Bomb]) = {
    val nextBombs = bombs.map { b =>
      this.bombs.find(_.id == b.id) match {
        case Some(existing) => existing.nextTurn
        case None           => b
      }
    }
    copy(
      bombs = nextBombs,
      enBombsSent = enBombsSent ++ nextBombs.filter(!_.mine).map(_.id),
      myBombsSent = myBombsSent ++ nextBombs.filter(_.mine).map(_.id))
  }

  def factoriesWhichCanReceiveBombAnytime: Set[Int] = {
    val myBombsTargets = bombs.collect { case Bomb(_, true, _, to, _, _) => to }
    val enBombsPossibleTargets = bombs.collect {
      case Bomb(_, false, from, _, _, time) =>
        factories
          .map(_.id)
          .filter(f => distance(from, f) >= time)
    }.flatten
    (myBombsTargets ++ enBombsPossibleTargets).toSet
  }

  def factoriesWhichCanReceiveBombNow: Vector[Int] = {
    factoriesWhichCanReceiveEnemyBombNow ++
      bombs.collect { case Bomb(_, _, _, to, 1, _) => to }
  }.distinct

  def factoriesWhichCanReceiveEnemyBombNow: Vector[Int] = {
    factories.collect {
      case Factory(id, mine, _, _, _) if bombs
        .filter(_.mine == false)
        .exists(b => grid.bombCanReachFactoriesThisTurn(b).contains(id)) => id
    }
  }
}