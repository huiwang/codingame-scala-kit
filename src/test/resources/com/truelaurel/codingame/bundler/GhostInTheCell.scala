import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
case class Edge(from: Int, to: Int, distance: Int)
case class Iti(distance: Int, path: Vector[Int])

object ShortestPath {
  def shortestItinearies(vertexCount: Int, edges: Vector[Edge]): Map[Int, Map[Int, Iti]] = {
    val n = vertexCount
    val inf = Int.MaxValue
    // Initialize distance matrix.
    val ds = Array.fill[Int](n, n)(inf)
    for (i <- 0 until n) ds(i)(i) = 0
    edges.foreach(e => ds(e.from)(e.to) = e.distance)
    // Initialize next vertex matrix.
    val ns = Array.fill[Int](n, n)(-1)
    // Here goes the magic!
    for (k <- 0 until n; i <- 0 until n; j <- 0 until n)
      if (ds(i)(k) != inf && ds(k)(j) != inf && ds(i)(k) + ds(k)(j) < ds(i)(j)) {
        ds(i)(j) = ds(i)(k) + ds(k)(j)
        ns(i)(j) = k
      }
    // Helper function to carve out paths from the next vertex matrix.
    def extractPath(path: ArrayBuffer[Int], i: Int, j: Int) {
      if (ds(i)(j) == inf) return
      val k = ns(i)(j)
      if (k != -1) {
        extractPath(path, i, k)
        path.append(k)
        extractPath(path, k, j)
      }
    }
    // Extract paths.
    val itenearies = mutable.Map[Int, Map[Int, Seq[Int]]]()
    for (i <- 0 until n) {
      val ps = mutable.Map[Int, Seq[Int]]()
      for (j <- 0 until n)
        if (ds(i)(j) != inf) {
          val p = new ArrayBuffer[Int]()
          p.append(i)
          if (i != j) {
            extractPath(p, i, j)
            p.append(j)
          }
          ps(j) = p
        }
      itenearies(i) = ps.toMap
    }
    // Return extracted paths.
    itenearies.map {
      case (from, dests) => {
        from -> dests.map {
          case (dest, path) => {
            val (totalDist, _) = path.foldLeft(0, from) {
              case ((total, pre), cur) =>
                (total + ds(pre)(cur), cur)
            }
            dest -> Iti(totalDist, path.toVector)
          }
        }
      }
    }.toMap
  }
}
class BreathFirstShortestPathFinder[T](graph: Map[T, Vector[T]], obstacles: Set[T]) {
  //elements at flood front and path toward it from search source
  type Paths = Map[T, Vector[T]]
  def findPath(source: T, target: T): Vector[T] = {
    findPaths(source, Set(target)).getOrElse(source, Vector.empty)
  }
  def findPaths(source: T, targets: Set[T]): Paths = {
    path(targets, Map(source -> Vector.empty), Set.empty, Map.empty).mapValues(_.reverse)
  }
  private def path(targets: Set[T], paths: Paths, visited: Set[T], found: Paths): Paths = {
    if (targets.isEmpty) {
      found
    } else {
      val reached = paths.keySet
      if (reached.subsetOf(visited)) {
        //nothing more to explore
        found
      } else {
        val reachedTargets = reached.intersect(targets)
        path(targets -- reachedTargets,
          newNeighborsWithHistory(paths, visited),
          reached ++ visited,
          found ++ paths.filterKeys(reachedTargets))
      }
    }
  }
  private def newNeighborsWithHistory(fringes: Paths, visited: Set[T]): Paths = {
    fringes.flatMap({
      case (elem, history) =>
        graph.getOrElse(elem, Vector.empty)
          .filterNot(visited)
          .filterNot(obstacles)
          .map(neighbor => (neighbor, neighbor +: history))
          .toMap
    })
  }
}
case class GhostGraph(factoryCount: Int, undirectedEdges: Vector[Edge]) {
  val edges: Vector[Edge] = undirectedEdges.flatMap(edge => Vector(edge, Edge(edge.to, edge.from, edge.distance)))
  val directDistances: Map[(Int, Int), Int] = edges.map(e => (e.from, e.to) -> e.distance).toMap
  val itineraries: Map[Int, Map[Int, Iti]] = ShortestPath.shortestItinearies(factoryCount, edges)
  val passThroughSegments: Iterable[Vector[Int]] = for {
    (from, toMap) <- itineraries
    (to, itinerary) <- toMap
    if from != to
    through = itinerary.path.tail.init
  } yield through
}

object GhostCellConstant {
  val MAX_TURN = 20
}
case class FactoryState(id: Int, owner: Int, cyborgs: Int, balance: Int = 0)
case class Fac(id: Int, owner: Int, cyborgs: Int, production: Int, again: Int) {
  require(cyborgs >= 0, "cyborgs can't be negative")
}
case class Entity(entityId: Int, entityType: String, arg1: Int, arg2: Int, arg3: Int, arg4: Int, arg5: Int)
case class Troop(id: Int, owner: Int, from: Int, to: Int, cyborgs: Int, arrival: Int) {
}
case class Bomb(id: Int, owner: Int, from: Int, to: Int, explosion: Int, observed: Int = 0) {
}
case class GhostCellGameState(factories: Vector[Fac],
                              troops: Vector[Troop],
                              bombs: Vector[Bomb],
                              turn: Int = 1,
                              oldBombBudget: Map[Int, Int] = Map(1 -> 2, -1 -> 2),
                              graph: GhostGraph) {
  val facValue: Map[Int, Double] = factories.map(fac => {
    val passThroughCount = graph.passThroughSegments.count(segment => segment.contains(fac.id))
    fac.id -> (fac.production + 0.01 * Math.pow(passThroughCount, 0.5) + 0.1)
  }).toMap
  val newBombBudget: Map[Int, Int] = for {
    (playerId, bombBudget) <- oldBombBudget
    used = bombs.count(b => fac(b.from).owner == playerId && b.observed == turn)
  } yield playerId -> (bombBudget - used)
  def dist(from: Int, to: Int): Int = graph.itineraries(from)(to).distance
  def transferFac(from: Int, to: Int): Int = graph.itineraries(from)(to).path.tail.head
  def directDist(from: Int, to: Int): Int = if (from == to) 0 else graph.directDistances((from, to))
  def center: Fac = factories(0)
  def fac(id: Int): Fac = factories(id)
}
trait GhostCellAction {
  def command(): String
}
case object WaitAction extends GhostCellAction {
  override def command(): String = "WAIT"
}
sealed case class MoveAction(from: Int, to: Int, cyborgs: Int) extends GhostCellAction {
  override def command(): String = s"MOVE $from $to $cyborgs"
}
sealed case class IncreaseAction(factoryId: Int) extends GhostCellAction {
  override def command(): String = s"INC $factoryId"
}
sealed case class BombAction(from: Int, to: Int) extends GhostCellAction {
  override def command(): String = s"BOMB $from $to"
}
sealed case class MessageAction(msg: String) extends GhostCellAction {
  override def command(): String = s"MSG $msg"
}
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
class PredictableGameLoop[C, S, A](
                                    controller: GameController[C, S, A],
                                    myPlayer: GamePlayer[S, A],
                                    otherPlayer: GamePlayer[S, A],
                                    arena: GameArena[S, A]
                                  ) {
  var predicated: S = _
  def run(): Unit = {
    val initContext = controller.readContext
    (1 to 200).foldLeft(initContext) {
      case (c, turn) =>
        val state = controller.readState(turn, c)
        if (predicated != null) {
          System.err.println("pre: " + predicated)
        }
        System.err.println("act: " + state)
        if (predicated != null && predicated != state) {
          System.err.println("wrong prediction")
          System.err.println("pre vs act: " + predicated.toString.diff(state.toString))
          System.err.println("act vs pre: " + state.toString.diff(predicated.toString))
        }
        val actions = myPlayer.reactTo(state)
        predicated = GameSimulator.singleTurn(state, arena, Vector(myPlayer, otherPlayer))
        actions.foreach(a => println(a))
        controller.nextContext(c, state, actions)
    }
  }
}

object CGLogger {
  private val info = 0
  private val debug = 1
  private val current = info
  def debug(message: Any): Unit = log(message, debug)
  def info(message: Any): Unit = log(message, info)
  private def log(message: Any, level: Int): Unit = {
    if (level <= current) {
      System.err.println(level)
    }
  }
}
class GameLoop[C, S, A](
                         controller: GameController[C, S, A],
                         myPlayer: GamePlayer[S, A],
                         turns: Int = 200
                       ) {
  def run(): Unit = {
    val time = System.nanoTime()
    val initContext = controller.readContext
    controller.warmup(myPlayer)
    CGLogger.info("GameInit elt: " + (System.nanoTime() - time) / 1000000 + "ms")
    (1 to turns).foldLeft(initContext) {
      case (c, turn) =>
        val state = controller.readState(turn, c)
        CGLogger.info(state)
        val time = System.nanoTime()
        val actions = myPlayer.reactTo(state)
        CGLogger.info("GameReact elt: " + (System.nanoTime() - time) / 1000000 + "ms")
        actions.foreach(a => println(a))
        controller.nextContext(c, state, actions)
    }
  }
}

trait GameController[C, S, A] {
  def readContext: C
  def readState(turn: Int, context: C): S
  def nextContext(context: C, state: S, actions: Vector[A]): C
  def warmup(player: GamePlayer[S, A]): Unit = {}
}
import java.util.concurrent.TimeUnit
trait GameState[A] {
  def apply(action: A): GameState[A]
}
trait GamePlayer[S, A] {
  def reactTo(state: S): Vector[A]
}
trait GameArena[S, A] {
  def next(fromState: S, actions: Vector[A]): S
  def judge(state: S): GameResult
}
trait GameResult {
}
case object Draw extends GameResult
case object WinKO extends GameResult
case object LossKO extends GameResult
case object WinTech extends GameResult
case object LossTech extends GameResult
object GameSimulator {
  
  def simulate[S, A](round: Int, from: S, arena: GameArena[S, A], players: Vector[GamePlayer[S, A]]): S = {
    (0 until round).foldLeft(from)((s, r) => {
      System.out.println("Round " + r)
      arena.next(s, players.flatMap(_.reactTo(s)))
    })
  }
  def singleTurn[S, A](from: S, arena: GameArena[S, A], players: Vector[GamePlayer[S, A]]): S = {
    arena.next(from, players.flatMap(_.reactTo(from)))
  }
  def evaluateOffline[S, A](games: Vector[S], arena: GameArena[S, A], players: Vector[GamePlayer[S, A]], round: Int = 200): Unit = {
    val time = System.nanoTime()
    println(s"Simulate ${games.size} Games")
    val results = games.zipWithIndex.par.map {
      case (game, indice) =>
        val result = play(game, arena, players, round)
        println(s"Game $indice $result")
        result
    }
    val wins = results.count {
      case WinKO | WinTech => true
      case _ => false
    }
    val loss = results.count {
      case LossKO | LossTech => true
      case _ => false
    }
    val draws = results.count {
      case Draw => true
      case _ => false
    }
    val p = wins.toDouble / games.size
    val winRate = p * 100.0
    val confidenceInterval = Math.sqrt(p * (1 - p) / games.size) * 100
    println("Battles elt: " + TimeUnit.NANOSECONDS.toSeconds(System.nanoTime() - time) + "s")
    println(s"${games.size} Games $wins Wins $loss Loss $draws Draws")
    println(s"WinRate:$winRate%+-$confidenceInterval%")
  }
  def play[S, A](from: S, arena: GameArena[S, A], players: Vector[GamePlayer[S, A]], round: Int = 200): GameResult = {
    val result = arena.judge(from)
    if (round == 0) result else {
      result match {
        case WinKO | LossKO => result
        case _ =>
          val next = arena.next(from, players.flatMap(_.reactTo(from)))
          play(next, arena, players, round - 1)
      }
    }
  }
}
case class GhostCellPlayer(me: Int) extends GamePlayer[GhostCellGameState, GhostCellAction] {
  val factoryAnalysis = FactoryAnalysis(me)
  override def reactTo(state: GhostCellGameState): Vector[GhostCellAction] = {
    val attackPlan = factoryAnalysis.movePlans(state)
    val attackMoves = attackPlan.map(m => {
      m.copy(to = state.transferFac(m.from, m.to))
    })
    val avoidBomb = attackMoves
      .filter(move => !state.bombs.filter(_.owner == me).exists(b => b.to == move.to && b.explosion == state.dist(move.from, move.to)))
      .filter(move => !state.bombs.filter(_.owner == -me).exists(b => {
        val dist = state.directDist(b.from, move.to)
        val travelled = state.turn - b.observed
        val arrival = dist - travelled
        state.factories(move.to).production > 0 && arrival == state.dist(move.from, move.to) + 1
      }))
    val increasable = if (factoryAnalysis.noIncrease(state)) {
      Vector.empty
    } else {
      state.factories.filter(_.owner == me)
        .filter(fac => fac.production < 3)
        .filter(fac => factoryAnalysis.moveAvailable(fac, state) >= 10)
        .filter(fac => (fac.cyborgs - avoidBomb.filter(_.from == fac.id).map(_.cyborgs).sum) >= 10)
    }
    withBombPlan(state, avoidBomb) ++ increasable.map(fac => IncreaseAction(fac.id))
  }
  private def withBombPlan(state: GhostCellGameState, moves: Vector[MoveAction]) = {
    val nextTroops = state.troops ++ factoryAnalysis.movesToTroops(moves, state)
    moves ++ bombPlan(state, nextTroops)
  }
  private def bombPlan(state: GhostCellGameState, nextTroops: Vector[Troop]): Vector[BombAction] = {
    if (!state.factories.exists(_.owner == me) || !state.factories.exists(_.owner == -me)) Vector.empty else {
      findFront(state).map(front => {
        state.factories.filter(_.owner == -me)
          .filter(fac => fac.production > 0 || fac.cyborgs > 5)
          .filter(fac => !state.bombs.filter(_.owner == me).exists(b => b.to == fac.id))
          .map(of => FactoryTimeline.finalState(of, nextTroops, state.directDist(front.id, of.id) + 1))
          .filter(fs => fs.owner == -me)
          .sortBy(fs => (state.factories(fs.id).production * -1, state.directDist(front.id, fs.id)))
          .take(state.newBombBudget(me))
          .map(fs => BombAction(front.id, fs.id))
      }).getOrElse(Vector.empty)
    }
  }
  private def findFront(state: GhostCellGameState): Option[Fac] = {
    if (state.center.owner == me) Some(state.center) else if (state.center.owner == -me) {
      Some(state.factories.filter(_.owner == me).minBy(fac => state.factories.filter(_.owner == -me).map(of => state.directDist(of.id, fac.id)).sum))
    } else None
  }
}
object Dichotomy {
  
  def search(low: Int, high: Int, lower: Int => Boolean): Int = {
    if (low == high) low else {
      val mid = (low + high) / 2
      if (lower(mid)) {
        search(low, mid, lower)
      } else {
        search(mid + 1, high, lower)
      }
    }
  }
}
case class FactoryAnalysis(me: Int) {
  def movePlans(state: GhostCellGameState): Vector[MoveAction] = {
    val (sources, toLost) = state.factories.filter(_.owner == me).partition(
      mf => FactoryTimeline.finalState(mf, state.troops).owner == me)
    val sourceBudget = sources.map(mf => mf.id -> moveAvailable(mf, state)).toMap
    val increasable = increaseableSources(state)
    val conquerable = state.factories.filter(fac =>
      if (noIncrease(state)) fac.owner != me && fac.production > 0 else fac.owner != me) ++ toLost.filter(_.production > 0)
    val conquerableToScoreRaw = conquerable.map(target => (target, evaluateFactoryConquer(target, sources, state, sourceBudget)))
    val increasableToScoreRaw = increasable.map(target => (target, evaluateFactoryInc(target, sources.filter(_.id != target.id), state, sourceBudget)))
    val targetToScoreSorted = (conquerableToScoreRaw ++ increasableToScoreRaw).sortBy(_._2).reverse
    val targetsSorted = targetToScoreSorted.map(_._1)
    val initPlan: Vector[MoveAction] = Vector.empty
    val moves = targetsSorted.foldLeft(initPlan) {
      case (totalMoves, sink) =>
        val sourcesSorted = sources.filter(_.id != sink.id).sortBy(src => state.dist(src.id, sink.id))
        if (conquerable.contains(sink)) {
          planForConquer(sink, sourcesSorted, totalMoves, state, sourceBudget)
        } else if (increasable.contains(sink)) {
          planForInc(sink, sourcesSorted, totalMoves, state, sourceBudget) :+ MoveAction(sink.id, sink.id, sink.cyborgs.min(10))
        } else {
          totalMoves
        }
    }
    val remainingMoves = (for {
      (facId, budget) <- sourceBudget
      remaining = budget - moves.filter(_.from == facId).map(_.cyborgs).sum
      if remaining > 0
      if moves.nonEmpty
      best = moves.last.to
    } yield MoveAction(facId, best, remaining)).toVector
    (moves ++ remainingMoves).filter(m => m.from != m.to)
  }
  def noIncrease(state: GhostCellGameState): Boolean = state.bombs.count(_.owner == -me) > 0 || state.center.owner == 0
  def increaseableSources(state: GhostCellGameState): Vector[Fac] = {
    if (noIncrease(state)) Vector.empty else {
      state.factories.filter(_.owner == me).filter(mf => FactoryTimeline.finalState(mf, state.troops).owner == me)
        .filter(fac => fac.production < 3).filter(fac => !mayExplode(fac, state))
    }
  }
  def mayExplode(fac: Fac, state: GhostCellGameState): Boolean = {
    state.bombs
      .filter(_.owner == -me)
      .exists(b => {
        val dist = state.directDist(b.from, fac.id)
        val travelled = state.turn - b.observed
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
    val sourcesSorted = sources.filter(_.id != sink.id).sortBy(src => state.dist(src.id, sink.id))
    val moves = planForConquer(sink, sourcesSorted, Vector.empty, state, availability)
    val investments = moves.map(m => m.cyborgs * Math.pow(2, state.dist(m.from, m.to))).sum
    state.facValue(sink.id) / (1.0 + investments)
  }
  private def evaluateFactoryInc(sink: Fac, sources: Vector[Fac],
                                 state: GhostCellGameState, availability: Map[Int, Int]) = {
    val sourcesSorted = sources.filter(_.id != sink.id).sortBy(src => state.dist(src.id, sink.id))
    val moves = planForInc(sink, sourcesSorted, Vector.empty, state, availability)
    val investments = (moves :+ MoveAction(sink.id, sink.id, sink.cyborgs)).map(m => m.cyborgs * Math.pow(2, state.dist(m.from, m.to))).sum
    state.facValue(sink.id) / (1.0 + investments)
  }
  def movesToTroops(moves: Vector[MoveAction], state: GhostCellGameState): Vector[Troop] = {
    moves.map(m => Troop(id = Int.MaxValue, owner = state.factories(m.from).owner, m.from, m.to,
      m.cyborgs, state.dist(m.from, m.to) + 1))
  }
  def incAvailable(src: Fac, state: GhostCellGameState): Int = {
    val neighborMoves = state.factories.filter(_.owner == -me)
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
    state.factories.filter(_.owner == -me)
      .filter(fac => fac.production < src.production && state.dist(fac.id, src.id) <= dist)
      .map(fac => MoveAction(fac.id, src.id, fac.cyborgs))
  }
  def available(src: Fac, troops: Vector[Troop], lower: Int, upper: Int): Int = {
    src.cyborgs - Dichotomy.search(lower, upper, cyborgs => {
      FactoryTimeline.finalState(src.copy(cyborgs = cyborgs), troops).owner == me
    })
  }
  
  def conquer(to: Fac, from: Fac, troops: Vector[Troop], state: GhostCellGameState): Int = {
    conquer(to, from, troops, state, 0, from.cyborgs)
  }
  def conquer(to: Fac, from: Fac, troops: Vector[Troop], state: GhostCellGameState,
              low: Int, high: Int): Int = {
    Dichotomy.search(low, high, cyborgs => {
      val troop = Troop(id = Int.MaxValue,
        owner = from.owner,
        from = from.id,
        to = to.id,
        cyborgs = cyborgs,
        arrival = state.dist(from.id, to.id) + 1)
      FactoryTimeline.finalState(to, troops :+ troop).owner == me
    })
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
    Dichotomy.search(lower, upper, cyborgs => {
      val troop = Troop(id = Int.MaxValue,
        owner = from.owner,
        from = from.id,
        to = to.id,
        cyborgs = cyborgs,
        arrival = state.dist(from.id, to.id) + 1)
      val finalState = FactoryTimeline.finalState(to, troops :+ troop, troop.arrival)
      finalState.owner == me && finalState.cyborgs * me >= 10
    })
  }
}
object Player extends App {
  val factoryCount = io.StdIn.readInt()
  val edgeCount = io.StdIn.readInt()
  var edges: Vector[Edge] = (for {
    i <- 0 until edgeCount
    Array(factory1, factory2, distance) = for (i <- io.StdIn.readLine() split " ") yield i.toInt
  } yield Edge(factory1, factory2, distance)).toVector
  private val ghostGraph = GhostGraph(factoryCount, edges)
  private val player = GhostCellPlayer(1)
  private var factoryProd: Map[Int, Int] = Map.empty
  private var turn = 1
  private var bombObserved: Map[Int, Int] = Map.empty
  private var bombBudget: Map[Int, Int] = Map(1 -> 2, -1 -> 2)
  while (true) {
    val entityCount = io.StdIn.readInt()
    val entities = (for {
      i <- 0 until entityCount
      Array(entityid, entitytype, arg1, arg2, arg3, arg4, arg5) = io.StdIn.readLine().split(" ")
    } yield Entity(entityid.toInt, entitytype, arg1.toInt, arg2.toInt, arg3.toInt, arg4.toInt, arg5.toInt)).toVector
    val factories = entities.filter(_.entityType == "FACTORY").map(
      e => Fac(id = e.entityId, owner = e.arg1, cyborgs = e.arg2, production = factoryProd.getOrElse(e.entityId, 0).max(e.arg3), again = e.arg4))
    factoryProd = factories.map(fac => fac.id -> fac.production.max(factoryProd.getOrElse(fac.id, 0))).toMap
    val troops = entities.filter(_.entityType == "TROOP").map(
      e => Troop(id = e.entityId, owner = e.arg1, from = e.arg2, to = e.arg3, cyborgs = e.arg4, arrival = e.arg5))
    val bombs = entities.filter(_.entityType == "BOMB").map(
      e => Bomb(id = e.entityId, owner = e.arg1, from = e.arg2, to = e.arg3, explosion = e.arg4, observed = bombObserved.getOrElse(e.entityId, turn)))
    bombObserved = bombObserved ++ bombs.map(b => b.id -> b.observed)
    val state = GhostCellGameState(factories = factories, troops = troops, bombs = bombs, turn = turn, oldBombBudget = bombBudget, graph = ghostGraph)
    System.err.println(state)
    val actions = player.reactTo(state)
    if (actions.isEmpty) {
      println(WaitAction.command())
    } else {
      println(actions.map(a => a.command()).mkString(";"))
    }
    bombBudget = state.newBombBudget
    turn = turn + 1
  }
}