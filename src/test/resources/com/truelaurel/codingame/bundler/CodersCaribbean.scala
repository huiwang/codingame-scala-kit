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

case class FixedCabribbeanPlayer(playerId: Int, otherPlayer: Int) extends GamePlayer[CaribbeanState, CaribbeanAction] {
  override def reactTo(state: CaribbeanState): Vector[CaribbeanAction] = {
    val otherShips = state.shipsOf(otherPlayer)
    state.shipsOf(playerId).map(ship => {
      if (otherShips.isEmpty) Wait(ship.id) else {
        val target = otherShips.minBy(other => other.center.distanceTo(ship.center)).center.toOffset
        Fire(ship.id, target)
      }
    })
  }
}

case class Offset(x : Int, y : Int) {
  def toCube: Cube = {
    val xp = x - (y - (y & 1)) / 2
    val zp = y
    val yp = -(xp + zp)
    Cube(xp, yp, zp)
  }
  def angle(target: Offset): Double = {
    val dy = (target.y - this.y) * Math.sqrt(3) / 2
    val dx = target.x - this.x + ((this.y - target.y) & 1) * 0.5
    var angle = -Math.atan2(dy, dx) * 3 / Math.PI
    if (angle < 0) angle += 6
    else if (angle >= 6) angle -= 6
    angle
  }
}

case class Cube(x: Int, y: Int, z: Int) {
  def toOffset: Offset = {
    val newX = x + (z - (z & 1)) / 2
    val newY = z
    Offset(newX, newY)
  }
  def neighbor(orientation: Int): Cube = {
    Cube(x + Cube.directions(orientation)(0),
      y + Cube.directions(orientation)(1),
      z + Cube.directions(orientation)(2))
  }
  def distanceTo(that: Cube): Int =
    (Math.abs(x - that.x) + Math.abs(y - that.y) + Math.abs(z - that.z)) / 2
}
object Cube {
  val directions: Array[Array[Int]] = Array[Array[Int]](
    Array(1, -1, 0),
    Array(+1, 0, -1),
    Array(0, +1, -1),
    Array(-1, +1, 0),
    Array(-1, 0, +1),
    Array(0, -1, +1))
  def apply(x: Int, y: Int): Cube = {
    Offset(x, y).toCube
  }
}

object CollisionAnalysis {
  def hitMyself(ship: Ship): Cube = {
    ship.speed match {
      case 0 => ship.center
      case 1 => ship.nextBow
      case 2 => throw new IllegalStateException("unable to hit myself")
    }
  }
  def travelTime(distance: Int): Int = {
    (1 + (distance / 3.0).round).toInt
  }
  def collisionTime(ship: Ship, cube: Cube): Int = {
    val distance = ship.center.distanceTo(cube)
    val angle = CaribbeanContext.angle(ship.center, cube)
    val diff = (ship.orientation - angle).abs
    val realDiff = diff.min(6 - diff)
    if (realDiff == 0) {
      distance - 1
    } else {
      stop(ship) + realDiff + distance - 1
    }
  }
  def canMine(one: Ship, other: Ship): Boolean = {
    val center = other.nextCenter
    if (CaribbeanContext.inside(center.toOffset)) {
      val nextOther = other.copy(position = center.toOffset)
      nextOther.zone.contains(one.mine)
    } else {
      false
    }
  }
  private def stop(ship: Ship): Int = ship.speed match {
    case 0 => 0
    case 1 => 1
    case 2 => 3
    case _ => throw new IllegalArgumentException("unknown speed")
  }
}

case class CaribbeanContext(mines: Map[Int, Mine], lastFire: Map[Int, Int]) {
}
object CaribbeanContext {
  val highMineDamage = 25
  val lowMineDamage = 10
  val lowBallDamage = 25
  val highBallDamage = 50
  val fireMaxDistance = 10
  val width = 23
  val height = 21
  val me = 1
  val other = 0
  val maxRums = 150
  private val orientations = (0 until 6).toVector
  private val cubes: Vector[Cube] = (for {
    x <- 0 until width
    y <- 0 until height
  } yield Offset(x, y).toCube).toVector
  private val cubeInfo: Map[Cube, (Set[Cube], Map[Int, Set[Cube]], Vector[Cube], Map[Cube, Int])] = cubes
    .map(cube => {
      val neighbors = (0 to 5).map(cube.neighbor).filter(cubes.contains).toSet
      val oriToZone = orientations.map(ori =>
        ori -> Set(cube.neighbor(ori), cube.neighbor((ori + 3) % 6))
      ).toMap
      val reachable = cubes.filter(_.distanceTo(cube) <= 5)
      val angles = cubes.map(ic => ic -> cube.toOffset.angle(ic.toOffset).ceil.toInt).toMap
      cube -> (neighbors, oriToZone, reachable, angles)
    }).toMap
  def shipZone(cube: Cube, orientation: Int): Set[Cube] = cubeInfo(cube)._2(orientation)
  def apply(): CaribbeanContext = CaribbeanContext(Map.empty, Map.empty)
  def reachable(cube: Cube): Vector[Cube] = cubeInfo(cube)._3
  def toCube(offset: Offset): Cube = offset.toCube
  def neighbors(cube: Cube): Set[Cube] = cubeInfo(cube)._1
  def angle(one: Cube, other: Cube) : Int = cubeInfo(one)._4(other)
  def inside(offset: Offset) : Boolean = {
    offset.x >= 0 && offset.x < width && offset.y >= 0 && offset.y < height
  }
}
case class Ship(id: Int, position: Offset, orientation: Int, speed: Int, rums: Int, owner: Int) {
  val center: Cube = CaribbeanContext.toCube(position)
  val bowAndStern: Set[Cube] = CaribbeanContext.shipZone(center, orientation)
  lazy val zone: Set[Cube] = bowAndStern + center
  val bow: Cube = center.neighbor(orientation)
  val stern: Cube = center.neighbor((orientation + 3) % 6)
  def mine : Cube = stern.neighbor((orientation + 3) % 6)
  def nextBow: Cube = speed match {
    case 0 => bow
    case 1 => bow.neighbor(orientation)
    case 2 => bow.neighbor(orientation).neighbor(orientation)
  }
  def nextCenter: Cube = speed match {
    case 0 => center
    case 1 => bow
    case 2 => bow.neighbor(orientation)
  }
  lazy val nextNextCenter: Cube = speed match {
    case 0 => center
    case 1 => nextBow
    case 2 => nextCenter.neighbor(orientation).neighbor(orientation)
  }
}
case class Barrel(id: Int, position: Offset, rums: Int) {
  def cube: Cube = CaribbeanContext.toCube(position)
}
case class Ball(id: Int, target: Offset, owner: Int, land: Int) {
  def cube: Cube = CaribbeanContext.toCube(target)
}
case class Mine(id: Int, position: Offset) {
  def cube: Cube = CaribbeanContext.toCube(position)
}
case class CaribbeanState(context: CaribbeanContext,
                          ships: Map[Int, Ship],
                          barrels: Map[Int, Barrel],
                          balls: Map[Int, Ball],
                          mines: Map[Int, Mine],
                          turn: Int) {
  def shipsOf(owner: Int): Vector[Ship] = ships.values.filter(_.owner == owner).toVector.sortBy(_.id)
}
trait CaribbeanAction {
  def shipId: Int
}
sealed case class Move(shipId: Int, offset: Offset) extends CaribbeanAction {
  override def toString: String = s"MOVE ${offset.x} ${offset.y}"
}
sealed case class Slower(shipId: Int) extends CaribbeanAction {
  override def toString: String = "SLOWER"
}
sealed case class Faster(shipId: Int) extends CaribbeanAction {
  override def toString: String = "FASTER"
}
sealed case class Port(shipId: Int) extends CaribbeanAction {
  override def toString: String = "PORT"
}
sealed case class Starboard(shipId: Int) extends CaribbeanAction {
  override def toString: String = "STARBOARD"
}
sealed case class Wait(shipId: Int) extends CaribbeanAction {
  override def toString: String = "WAIT"
}
sealed case class Fire(shipId: Int, offset: Offset) extends CaribbeanAction {
  override def toString: String = s"FIRE ${offset.x} ${offset.y}"
}
sealed case class MineAction(shipId: Int) extends CaribbeanAction {
  override def toString: String = s"MINE"
}
import scala.io.StdIn

object CaribbeanController extends GameController[CaribbeanContext, CaribbeanState, CaribbeanAction] {
  override def readContext: CaribbeanContext = {
    CaribbeanContext()
  }
  override def readState(turn: Int, context: CaribbeanContext): CaribbeanState = {
    val shipCount = StdIn.readInt()
    val entities = for {
      i <- 0 until StdIn.readInt()
      line: String = StdIn.readLine()
    } yield line
    var ships: Map[Int, Ship] = Map.empty
    var barrels: Map[Int,Barrel] = Map.empty
    var balls: Map[Int,Ball] = Map.empty
    var mines: Map[Int,Mine] = Map.empty
    for {
      line <- entities
      Array(entityId, entityType, x, y, arg1, arg2, arg3, arg4) = line.split(" ")
    } {
      val id = entityId.toInt
      val offset = Offset(x.toInt, y.toInt)
      val a1 = arg1.toInt
      val a2 = arg2.toInt
      entityType match {
        case "SHIP" => ships = ships.updated(id, Ship(id, offset, a1, a2, arg3.toInt, arg4.toInt))
        case "BARREL" => barrels = barrels.updated(id, Barrel(id, offset, a1))
        case "CANNONBALL" => balls = balls.updated(id, Ball(id, offset, a1, a2))
        case "MINE" => mines = mines.updated(id, Mine(id, offset))
      }
    }
    CaribbeanState(context, ships, barrels, balls, mines, turn)
  }
  override def nextContext(context: CaribbeanContext, state: CaribbeanState, actions: Vector[CaribbeanAction]): CaribbeanContext = {
    context
  }
  override def warmup(player: GamePlayer[CaribbeanState, CaribbeanAction]): Unit = {
    val state = CaribbeanState(CaribbeanContext(Map(),Map()),Map(0 -> Ship(0,Offset(2,2),2,0,100,1), 5 -> Ship(5,Offset(21,14),2,0,100,0), 1 -> Ship(1,Offset(2,18),4,0,100,0), 2 -> Ship(2,Offset(8,4),5,0,100,1), 3 -> Ship(3,Offset(8,16),1,0,100,0), 4 -> Ship(4,Offset(21,6),4,0,100,1)),Map(24 -> Barrel(24,Offset(2,4),20), 37 -> Barrel(37,Offset(13,12),10), 25 -> Barrel(25,Offset(2,16),20), 20 -> Barrel(20,Offset(15,3),14), 29 -> Barrel(29,Offset(6,15),13), 28 -> Barrel(28,Offset(6,5),13), 38 -> Barrel(38,Offset(1,3),14), 21 -> Barrel(21,Offset(15,17),14), 33 -> Barrel(33,Offset(13,16),18), 32 -> Barrel(32,Offset(13,4),18), 34 -> Barrel(34,Offset(6,2),12), 17 -> Barrel(17,Offset(16,11),15), 22 -> Barrel(22,Offset(14,3),15), 27 -> Barrel(27,Offset(1,11),14), 39 -> Barrel(39,Offset(1,17),14), 35 -> Barrel(35,Offset(6,18),12), 18 -> Barrel(18,Offset(16,6),13), 16 -> Barrel(16,Offset(16,9),15), 31 -> Barrel(31,Offset(19,17),11), 26 -> Barrel(26,Offset(1,9),14), 23 -> Barrel(23,Offset(14,17),15), 36 -> Barrel(36,Offset(13,8),10), 30 -> Barrel(30,Offset(19,3),11), 19 -> Barrel(19,Offset(16,14),13)),Map(),Map(6 -> Mine(6,Offset(20,6)), 13 -> Mine(13,Offset(9,7))),1)
    (0 until 1).foreach(i => player.reactTo(state))
  }
}
import java.util.concurrent.TimeUnit

trait Stopper {
  def start(): Unit
  def willOutOfTime: Boolean
}

class CountStopper(count: Int) extends Stopper {
  private var remaining = count
  override def start(): Unit = {}
  override def willOutOfTime: Boolean = {
    remaining = remaining - 1
    remaining < 0
  }
}
import scala.concurrent.duration.Duration

class Chronometer(duration: Duration) extends Stopper {
  val budget: Long = duration.toNanos
  private var startTime: Long = 0
  private var elapsed: Long = 0
  private var maxTurnElapsed: Long = -1
  override def start(): Unit = {
    startTime = mark
    maxTurnElapsed = -1
    elapsed = 0
  }
  def mark: Long = {
    System.nanoTime()
  }
  override def willOutOfTime: Boolean = {
    val untilNow = mark - startTime
    val currentTurnElapsed = untilNow - elapsed
    maxTurnElapsed = Math.max(maxTurnElapsed, currentTurnElapsed)
    val remaining = budget - untilNow
    elapsed = untilNow
    //System.err.println(s"remaining $remaining max $maxTurnElapsed")
    remaining < maxTurnElapsed
  }
}

object CaribbeanPlayerDebug {
  def main(args: Array[String]): Unit = {
    val state = CaribbeanState(CaribbeanContext(Map(),Map(2 -> 50, 0 -> 39)),Map(0 -> Ship(0,Offset(8,12),1,2,64,1), 2 -> Ship(2,Offset(10,11),2,1,50,1), 1 -> Ship(1,Offset(20,10),4,2,67,0), 3 -> Ship(3,Offset(17,16),5,2,59,0)),Map(),Map(59 -> Ball(59,Offset(12,14),3,1), 60 -> Ball(60,Offset(14,17),2,2)),Map(5 -> Mine(5,Offset(14,12)), 29 -> Mine(29,Offset(7,8)), 61 -> Mine(61,Offset(15,12)), 38 -> Mine(38,Offset(4,10)), 7 -> Mine(7,Offset(12,16)), 51 -> Mine(51,Offset(14,10)), 4 -> Mine(4,Offset(14,8))),51)
    val player = CaribbeanPlayer(1, 0, new CountStopper(100))
    println(player.reactTo(state))
    println(player.reactTo(state))
  }
}
import java.util.concurrent.TimeUnit

trait Solution {
  def quality() : Double
}

trait Problem[S <: Solution] {
  def randomSolution(): S
  def tweakSolution(solution: S): S
}
import scala.concurrent.duration.Duration

class MuPlusLambda(mu: Int, lambda: Int, stopper: Stopper) {
  require(lambda > 0)
  require(mu > 0 && mu <= lambda)
  private val parentsRange = 0 until lambda
  private val tweakedRange = 0 until lambda / mu
  def search[S <: Solution](problem: Problem[S]): S = {
    stopper.start()
    var parents = parentsRange
      .map(_ => problem.randomSolution())
      .map(s => (s, s.quality()))
      .sortBy(_._2)
      .map(_._1)
    var bestSolution = parents.last
    while (!stopper.willOutOfTime) {
      //truncation selection
      val greatest = parents
        .map(s => (s, s.quality()))
        .sortBy(_._2)
        .map(_._1)
        .takeRight(mu)
      bestSolution = if(bestSolution.quality() > greatest.last.quality()) {
        bestSolution
      } else greatest.last
      parents = greatest ++ greatest.flatMap(s => tweakedRange.map(_ => problem.tweakSolution(s)))
    }
    bestSolution
  }
}
import scala.util.Random

object Mathl {
  val random = new Random(62638886242411L)
  def halfUp(d: Double): Int = ((d.abs * 2 + 1) / 2).toInt * (if (d > 0) 1 else -1)
  def almostEqual(d1: Double, d2: Double): Boolean = Math.abs(d1 - d2) <= 0.000001
  def randomBetween(min: Double, max: Double): Double = {
    min + random.nextDouble() * (max - min)
  }
}

object NoiseGenerators {
  type G = () => Double
  def uniform(halfRange: Double, center : Double = 0): G = () => center + Mathl.randomBetween(-halfRange, halfRange)
  def gaussian(mean: Double, stdDerivation: Double): G = () => mean + stdDerivation * Mathl.random.nextGaussian()
}

class BoundedVectorConvolution(noiseProbability: Double, min: Double, max: Double) {
  def tweak(v: Vector[Double], noiseGenerator: () => Double): Vector[Double] = {
    v.map(elem => {
      if (noiseProbability >= Mathl.random.nextDouble()) {
        var tweaked = elem
        do {
          tweaked = noiseGenerator.apply() + elem
        } while (tweaked < min || tweaked > max)
        tweaked
      } else {
        elem
      }
    })
  }
}
import scala.concurrent.duration.Duration

case class BestCaribbeanPlayer(me: Int, other: Int,
                           stopper: Stopper = new Chronometer(Duration(46, TimeUnit.MILLISECONDS))
                          ) extends GamePlayer[CaribbeanState, CaribbeanAction] {
  override def reactTo(state: CaribbeanState): Vector[CaribbeanAction] = {
    val myShips = state.shipsOf(me)
    if (state.barrels.isEmpty && myShips.size > 1) {
      val weakest = myShips.minBy(_.rums)
      val closestToWeakest = myShips.filter(_.id != weakest.id).map(s => weakest.center.distanceTo(s.center)).min
      if (closestToWeakest <= 3) {
        myShips.map(s => if (s.id == weakest.id) Fire(s.id, weakest.nextCenter.toOffset) else Wait(s.id))
      } else {
        simule(state)
      }
    } else {
      simule(state)
    }
  }
  private def simule(state: CaribbeanState) = {
    val muToLambda = new MuPlusLambda(1, 3, stopper)
    val solution = muToLambda.search(BestCaribbeanProblem(me, other, FixedCabribbeanPlayer(other, me), state))
    solution.toActions
  }
}
case class BestCaribbeanProblem(me: Int,
                                other: Int,
                                otherPlayer: GamePlayer[CaribbeanState, CaribbeanAction],
                                state: CaribbeanState) extends Problem[BestCaribbeanSolution] {
  private val convolution = new BoundedVectorConvolution(0.9, 0, 10)
  private val roundsToSimulate = 3
  val rounds: Range = 0 until roundsToSimulate
  val actionLength: Int = state.shipsOf(me).size
  val chromosome: Range = 0 until (roundsToSimulate * actionLength)
  override def randomSolution(): BestCaribbeanSolution = {
    BestCaribbeanSolution(this, chromosome.map(_ => NoiseGenerators.uniform(5, 5).apply()).toVector)
  }
  override def tweakSolution(solution: BestCaribbeanSolution): BestCaribbeanSolution = {
    solution.copy(actions = convolution.tweak(solution.actions,
      NoiseGenerators.gaussian(mean = 0, stdDerivation = 1)))
  }
}
case class BestCaribbeanSolution(problem: BestCaribbeanProblem,
                                 actions: Vector[Double]) extends Solution {
  val quality: Double = {
    val simulatedState = targetState()
    val otherShips = simulatedState.shipsOf(problem.other)
    val myShips = simulatedState.shipsOf(problem.me)
    val otherScore = 0.001 * otherShips.map(ship => {
      ship.rums
    }).sum
    if (problem.state.barrels.isEmpty && myShips.size > 1 && myShips.exists(_.rums < 50)) {
      myShips.map(ship => {
        val shipValues = myShips.map(ms => {
          ms.rums * Math.pow(0.5, ms.center.distanceTo(ship.center))
        }).sum
        val freeHex = CaribbeanContext.reachable(ship.center).size
        ship.rums + 0.0001 * shipValues
      }).sum - otherScore
    } else {
      myShips.map(ship => {
        val barrelValues = simulatedState.barrels.values
          .map(b => b.rums * Math.pow(0.95, b.cube.distanceTo(ship.center))).sum
        val freeHex = CaribbeanContext.reachable(ship.center).size
        ship.rums + 0.001 * barrelValues + 0.0001 * freeHex
      }).sum - otherScore
    }
  }
  def targetState(): CaribbeanState = {
    problem.rounds.foldLeft(problem.state)((s, r) => {
      val shipActions = actions.slice(r * problem.actionLength, (r + 1) * problem.actionLength)
      val adapted = adapt(s, shipActions)
      CaribbeanArena.next(s, adapted ++ problem.otherPlayer.reactTo(s))
    })
  }
  def adapt(state: CaribbeanState, shipActions: Vector[Double]): Vector[CaribbeanAction] = {
    val myShips = state.shipsOf(problem.me)
    myShips.indices.map(i => {
      val shipId = myShips(i).id
      val x = shipActions(i)
      x match {
        case _ if x > 9 || x < 1 => Port(shipId)
        case _ if x > 8 || x < 2 => Starboard(shipId)
        case _ if x > 7 || x < 3 => Faster(shipId)
        case _ if x > 6 || x < 4 => Slower(shipId)
        case y =>
          val ship = myShips(i)
          val targets: Vector[Cube] = state.ships.values
            .filter(s => s.id != ship.id
              && (s.owner != problem.me)
              && ship.nextCenter.distanceTo(s.center) <= CaribbeanContext.fireMaxDistance).map(_.nextCenter).toVector
          if (targets.isEmpty) Wait(shipId) else {
            val target = targets((y * 100).toInt % targets.size)
            Fire(shipId, target.toOffset)
          }
      }
    }).toVector
  }
  def toActions: Vector[CaribbeanAction] = {
    adapt(problem.state, actions.take(problem.actionLength))
  }
}

object CaribbeanOfflineBattles {
  def main(args: Array[String]): Unit = {
    val games = Vector(
      CaribbeanState(CaribbeanContext(Map(), Map()), Map(0 -> Ship(0, Offset(5, 7), 1, 0, 100, 1), 2 -> Ship(2, Offset(19, 2), 5, 0, 100, 1), 1 -> Ship(1, Offset(5, 13), 5, 0, 100, 0), 3 -> Ship(3, Offset(19, 18), 1, 0, 100, 0)), Map(10 -> Barrel(10, Offset(3, 10), 15), 24 -> Barrel(24, Offset(14, 12), 14), 25 -> Barrel(25, Offset(21, 5), 14), 14 -> Barrel(14, Offset(11, 17), 17), 20 -> Barrel(20, Offset(14, 15), 20), 21 -> Barrel(21, Offset(21, 6), 13), 13 -> Barrel(13, Offset(11, 3), 17), 17 -> Barrel(17, Offset(5, 1), 16), 22 -> Barrel(22, Offset(21, 14), 13), 12 -> Barrel(12, Offset(20, 14), 19), 18 -> Barrel(18, Offset(5, 19), 16), 16 -> Barrel(16, Offset(6, 16), 20), 11 -> Barrel(11, Offset(20, 6), 19), 26 -> Barrel(26, Offset(21, 15), 14), 23 -> Barrel(23, Offset(14, 8), 14), 19 -> Barrel(19, Offset(14, 5), 20), 15 -> Barrel(15, Offset(6, 4), 20)), Map(), Map(4 -> Mine(4, Offset(16, 3)), 8 -> Mine(8, Offset(1, 7))), 1)
      , CaribbeanState(CaribbeanContext(Map(), Map()), Map(0 -> Ship(0, Offset(1, 4), 2, 0, 100, 1), 2 -> Ship(2, Offset(19, 1), 1, 0, 100, 1), 1 -> Ship(1, Offset(1, 16), 4, 0, 100, 0), 3 -> Ship(3, Offset(19, 19), 5, 0, 100, 0)), Map(14 -> Barrel(14, Offset(2, 2), 14), 20 -> Barrel(20, Offset(9, 4), 20), 21 -> Barrel(21, Offset(9, 16), 20), 13 -> Barrel(13, Offset(6, 10), 12), 17 -> Barrel(17, Offset(21, 15), 19), 22 -> Barrel(22, Offset(9, 9), 16), 18 -> Barrel(18, Offset(9, 3), 14), 16 -> Barrel(16, Offset(21, 5), 19), 23 -> Barrel(23, Offset(9, 11), 16), 19 -> Barrel(19, Offset(9, 17), 14), 15 -> Barrel(15, Offset(2, 18), 14)), Map(), Map(10 -> Mine(10, Offset(19, 5))), 1)
      , CaribbeanState(CaribbeanContext(Map(), Map()), Map(0 -> Ship(0, Offset(3, 4), 4, 0, 100, 1), 1 -> Ship(1, Offset(3, 16), 2, 0, 100, 0)), Map(24 -> Barrel(24, Offset(16, 10), 13), 25 -> Barrel(25, Offset(2, 10), 13), 14 -> Barrel(14, Offset(16, 18), 10), 20 -> Barrel(20, Offset(4, 13), 17), 29 -> Barrel(29, Offset(18, 9), 16), 28 -> Barrel(28, Offset(10, 17), 14), 13 -> Barrel(13, Offset(16, 2), 10), 32 -> Barrel(32, Offset(17, 12), 10), 17 -> Barrel(17, Offset(10, 9), 12), 22 -> Barrel(22, Offset(18, 4), 18), 27 -> Barrel(27, Offset(10, 3), 14), 12 -> Barrel(12, Offset(7, 14), 16), 18 -> Barrel(18, Offset(10, 11), 12), 16 -> Barrel(16, Offset(3, 18), 16), 31 -> Barrel(31, Offset(17, 8), 10), 11 -> Barrel(11, Offset(7, 6), 16), 23 -> Barrel(23, Offset(18, 16), 18), 30 -> Barrel(30, Offset(18, 11), 16), 19 -> Barrel(19, Offset(4, 7), 17), 15 -> Barrel(15, Offset(3, 2), 16)), Map(), Map(), 1)
      , CaribbeanState(CaribbeanContext(Map(), Map()), Map(0 -> Ship(0, Offset(4, 8), 5, 0, 100, 1), 1 -> Ship(1, Offset(4, 12), 1, 0, 100, 0)), Map(24 -> Barrel(24, Offset(2, 11), 19), 25 -> Barrel(25, Offset(1, 2), 11), 14 -> Barrel(14, Offset(20, 12), 14), 20 -> Barrel(20, Offset(8, 13), 15), 28 -> Barrel(28, Offset(6, 13), 20), 21 -> Barrel(21, Offset(11, 4), 16), 13 -> Barrel(13, Offset(20, 8), 14), 17 -> Barrel(17, Offset(6, 5), 12), 22 -> Barrel(22, Offset(11, 16), 16), 27 -> Barrel(27, Offset(6, 7), 20), 18 -> Barrel(18, Offset(6, 15), 12), 16 -> Barrel(16, Offset(11, 14), 13), 26 -> Barrel(26, Offset(1, 18), 11), 23 -> Barrel(23, Offset(2, 9), 19), 19 -> Barrel(19, Offset(8, 7), 15), 15 -> Barrel(15, Offset(11, 6), 13)), Map(), Map(8 -> Mine(8, Offset(8, 8))), 1)
      , CaribbeanState(CaribbeanContext(Map(), Map()), Map(0 -> Ship(0, Offset(2, 6), 0, 0, 100, 1), 5 -> Ship(5, Offset(19, 14), 4, 0, 100, 0), 1 -> Ship(1, Offset(2, 14), 0, 0, 100, 0), 2 -> Ship(2, Offset(9, 1), 1, 0, 100, 1), 3 -> Ship(3, Offset(9, 19), 5, 0, 100, 0), 4 -> Ship(4, Offset(19, 6), 2, 0, 100, 1)), Map(24 -> Barrel(24, Offset(6, 9), 11), 25 -> Barrel(25, Offset(6, 11), 11), 14 -> Barrel(14, Offset(5, 3), 11), 20 -> Barrel(20, Offset(17, 1), 19), 21 -> Barrel(21, Offset(17, 19), 19), 17 -> Barrel(17, Offset(4, 11), 18), 22 -> Barrel(22, Offset(18, 9), 18), 27 -> Barrel(27, Offset(11, 19), 15), 18 -> Barrel(18, Offset(17, 2), 11), 16 -> Barrel(16, Offset(4, 9), 18), 26 -> Barrel(26, Offset(11, 1), 15), 23 -> Barrel(23, Offset(18, 11), 18), 19 -> Barrel(19, Offset(17, 18), 11), 15 -> Barrel(15, Offset(5, 17), 11)), Map(), Map(10 -> Mine(10, Offset(15, 4)), 6 -> Mine(6, Offset(11, 6)), 13 -> Mine(13, Offset(16, 11)), 12 -> Mine(12, Offset(16, 9)), 8 -> Mine(8, Offset(1, 1))), 1)
      , CaribbeanState(CaribbeanContext(Map(), Map()), Map(0 -> Ship(0, Offset(4, 2), 3, 0, 100, 1), 1 -> Ship(1, Offset(4, 18), 3, 0, 100, 0)), Map(24 -> Barrel(24, Offset(2, 2), 16), 25 -> Barrel(25, Offset(2, 18), 16), 14 -> Barrel(14, Offset(15, 18), 10), 20 -> Barrel(20, Offset(18, 19), 15), 29 -> Barrel(29, Offset(2, 16), 15), 28 -> Barrel(28, Offset(2, 4), 15), 33 -> Barrel(33, Offset(7, 15), 18), 13 -> Barrel(13, Offset(15, 2), 10), 32 -> Barrel(32, Offset(7, 5), 18), 34 -> Barrel(34, Offset(18, 4), 16), 17 -> Barrel(17, Offset(19, 9), 13), 22 -> Barrel(22, Offset(16, 5), 20), 27 -> Barrel(27, Offset(6, 12), 18), 12 -> Barrel(12, Offset(12, 10), 12), 35 -> Barrel(35, Offset(18, 16), 16), 18 -> Barrel(18, Offset(19, 11), 13), 16 -> Barrel(16, Offset(21, 16), 10), 31 -> Barrel(31, Offset(2, 12), 13), 26 -> Barrel(26, Offset(6, 8), 18), 23 -> Barrel(23, Offset(16, 15), 20), 30 -> Barrel(30, Offset(2, 8), 13), 19 -> Barrel(19, Offset(18, 1), 15), 15 -> Barrel(15, Offset(21, 4), 10)), Map(), Map(10 -> Mine(10, Offset(4, 1))), 1)
      , CaribbeanState(CaribbeanContext(Map(), Map()), Map(0 -> Ship(0, Offset(3, 2), 5, 0, 100, 1), 2 -> Ship(2, Offset(16, 2), 0, 0, 100, 1), 1 -> Ship(1, Offset(3, 18), 1, 0, 100, 0), 3 -> Ship(3, Offset(16, 18), 0, 0, 100, 0)), Map(24 -> Barrel(24, Offset(15, 4), 14), 25 -> Barrel(25, Offset(15, 16), 14), 14 -> Barrel(14, Offset(6, 4), 17), 20 -> Barrel(20, Offset(7, 12), 17), 29 -> Barrel(29, Offset(14, 13), 18), 28 -> Barrel(28, Offset(14, 7), 18), 21 -> Barrel(21, Offset(9, 8), 10), 13 -> Barrel(13, Offset(7, 14), 19), 17 -> Barrel(17, Offset(6, 18), 10), 22 -> Barrel(22, Offset(9, 12), 10), 27 -> Barrel(27, Offset(20, 11), 12), 12 -> Barrel(12, Offset(7, 6), 19), 18 -> Barrel(18, Offset(16, 10), 16), 16 -> Barrel(16, Offset(6, 2), 10), 31 -> Barrel(31, Offset(8, 17), 17), 26 -> Barrel(26, Offset(20, 9), 12), 23 -> Barrel(23, Offset(18, 10), 18), 30 -> Barrel(30, Offset(8, 3), 17), 19 -> Barrel(19, Offset(7, 8), 17), 15 -> Barrel(15, Offset(6, 16), 17)), Map(), Map(6 -> Mine(6, Offset(18, 3)), 8 -> Mine(8, Offset(16, 7))), 1)
      , CaribbeanState(CaribbeanContext(Map(), Map()), Map(0 -> Ship(0, Offset(1, 3), 2, 0, 100, 1), 2 -> Ship(2, Offset(16, 4), 1, 0, 100, 1), 1 -> Ship(1, Offset(1, 17), 4, 0, 100, 0), 3 -> Ship(3, Offset(16, 16), 5, 0, 100, 0)), Map(10 -> Barrel(10, Offset(6, 1), 15), 24 -> Barrel(24, Offset(14, 6), 14), 25 -> Barrel(25, Offset(14, 14), 14), 14 -> Barrel(14, Offset(10, 8), 16), 20 -> Barrel(20, Offset(5, 2), 18), 29 -> Barrel(29, Offset(20, 3), 10), 28 -> Barrel(28, Offset(3, 11), 19), 21 -> Barrel(21, Offset(5, 18), 18), 13 -> Barrel(13, Offset(13, 15), 16), 17 -> Barrel(17, Offset(9, 19), 11), 22 -> Barrel(22, Offset(7, 9), 15), 27 -> Barrel(27, Offset(3, 9), 19), 12 -> Barrel(12, Offset(13, 5), 16), 18 -> Barrel(18, Offset(16, 5), 18), 16 -> Barrel(16, Offset(9, 1), 11), 31 -> Barrel(31, Offset(9, 10), 15), 11 -> Barrel(11, Offset(6, 19), 15), 26 -> Barrel(26, Offset(7, 10), 16), 23 -> Barrel(23, Offset(7, 11), 15), 30 -> Barrel(30, Offset(20, 17), 10), 19 -> Barrel(19, Offset(16, 15), 18), 15 -> Barrel(15, Offset(10, 12), 16)), Map(), Map(8 -> Mine(8, Offset(5, 4))), 1)
      , CaribbeanState(CaribbeanContext(Map(), Map()), Map(0 -> Ship(0, Offset(12, 1), 1, 0, 100, 1), 1 -> Ship(1, Offset(12, 19), 5, 0, 100, 0)), Map(10 -> Barrel(10, Offset(17, 15), 11), 24 -> Barrel(24, Offset(13, 17), 15), 14 -> Barrel(14, Offset(11, 12), 13), 20 -> Barrel(20, Offset(15, 15), 20), 21 -> Barrel(21, Offset(18, 3), 20), 9 -> Barrel(9, Offset(17, 5), 11), 13 -> Barrel(13, Offset(11, 8), 13), 17 -> Barrel(17, Offset(11, 7), 14), 22 -> Barrel(22, Offset(18, 17), 20), 12 -> Barrel(12, Offset(4, 14), 15), 18 -> Barrel(18, Offset(11, 13), 14), 16 -> Barrel(16, Offset(21, 15), 19), 11 -> Barrel(11, Offset(4, 6), 15), 23 -> Barrel(23, Offset(13, 3), 15), 19 -> Barrel(19, Offset(15, 5), 20), 15 -> Barrel(15, Offset(21, 5), 19)), Map(), Map(4 -> Mine(4, Offset(10, 5))), 1)
      , CaribbeanState(CaribbeanContext(Map(), Map()), Map(0 -> Ship(0, Offset(3, 2), 3, 0, 100, 1), 2 -> Ship(2, Offset(14, 8), 3, 0, 100, 1), 1 -> Ship(1, Offset(3, 18), 3, 0, 100, 0), 3 -> Ship(3, Offset(14, 12), 3, 0, 100, 0)), Map(24 -> Barrel(24, Offset(8, 11), 17), 37 -> Barrel(37, Offset(1, 12), 19), 14 -> Barrel(14, Offset(15, 11), 15), 20 -> Barrel(20, Offset(21, 12), 17), 29 -> Barrel(29, Offset(17, 11), 18), 28 -> Barrel(28, Offset(17, 9), 18), 21 -> Barrel(21, Offset(13, 7), 18), 33 -> Barrel(33, Offset(3, 13), 20), 13 -> Barrel(13, Offset(15, 9), 15), 32 -> Barrel(32, Offset(3, 7), 20), 34 -> Barrel(34, Offset(19, 2), 15), 17 -> Barrel(17, Offset(11, 6), 12), 22 -> Barrel(22, Offset(13, 13), 18), 27 -> Barrel(27, Offset(16, 13), 10), 12 -> Barrel(12, Offset(2, 10), 15), 35 -> Barrel(35, Offset(19, 18), 15), 18 -> Barrel(18, Offset(11, 14), 12), 16 -> Barrel(16, Offset(17, 12), 19), 31 -> Barrel(31, Offset(17, 18), 17), 26 -> Barrel(26, Offset(16, 7), 10), 23 -> Barrel(23, Offset(8, 9), 17), 36 -> Barrel(36, Offset(1, 8), 19), 30 -> Barrel(30, Offset(17, 2), 17), 19 -> Barrel(19, Offset(21, 8), 17), 15 -> Barrel(15, Offset(17, 8), 19)), Map(), Map(4 -> Mine(4, Offset(1, 3)), 8 -> Mine(8, Offset(3, 3)), 11 -> Mine(11, Offset(11, 11)), 10 -> Mine(10, Offset(11, 9))), 1)
      , CaribbeanState(CaribbeanContext(Map(), Map()), Map(0 -> Ship(0, Offset(6, 5), 0, 0, 100, 1), 2 -> Ship(2, Offset(21, 2), 5, 0, 100, 1), 1 -> Ship(1, Offset(6, 15), 0, 0, 100, 0), 3 -> Ship(3, Offset(21, 18), 1, 0, 100, 0)), Map(24 -> Barrel(24, Offset(8, 17), 10), 25 -> Barrel(25, Offset(6, 7), 20), 14 -> Barrel(14, Offset(4, 9), 13), 20 -> Barrel(20, Offset(20, 3), 11), 29 -> Barrel(29, Offset(12, 6), 15), 28 -> Barrel(28, Offset(13, 17), 20), 21 -> Barrel(21, Offset(20, 17), 11), 13 -> Barrel(13, Offset(14, 18), 15), 17 -> Barrel(17, Offset(1, 16), 16), 27 -> Barrel(27, Offset(13, 3), 20), 12 -> Barrel(12, Offset(14, 2), 15), 18 -> Barrel(18, Offset(21, 4), 13), 16 -> Barrel(16, Offset(1, 4), 16), 26 -> Barrel(26, Offset(6, 13), 20), 23 -> Barrel(23, Offset(8, 3), 10), 30 -> Barrel(30, Offset(12, 14), 15), 19 -> Barrel(19, Offset(21, 16), 13), 15 -> Barrel(15, Offset(4, 11), 13)), Map(), Map(4 -> Mine(4, Offset(3, 5)), 10 -> Mine(10, Offset(3, 2))), 1)
      , CaribbeanState(CaribbeanContext(Map(), Map()), Map(0 -> Ship(0, Offset(3, 6), 5, 0, 100, 1), 5 -> Ship(5, Offset(17, 17), 5, 0, 100, 0), 1 -> Ship(1, Offset(3, 14), 1, 0, 100, 0), 2 -> Ship(2, Offset(9, 8), 2, 0, 100, 1), 3 -> Ship(3, Offset(9, 12), 4, 0, 100, 0), 4 -> Ship(4, Offset(17, 3), 1, 0, 100, 1)), Map(24 -> Barrel(24, Offset(21, 12), 16), 20 -> Barrel(20, Offset(13, 18), 15), 29 -> Barrel(29, Offset(13, 16), 14), 28 -> Barrel(28, Offset(13, 4), 14), 21 -> Barrel(21, Offset(16, 1), 15), 17 -> Barrel(17, Offset(6, 11), 19), 22 -> Barrel(22, Offset(16, 19), 15), 27 -> Barrel(27, Offset(10, 12), 10), 16 -> Barrel(16, Offset(6, 9), 19), 26 -> Barrel(26, Offset(10, 8), 10), 23 -> Barrel(23, Offset(21, 8), 16), 19 -> Barrel(19, Offset(13, 2), 15)), Map(), Map(8 -> Mine(8, Offset(20, 8)), 10 -> Mine(10, Offset(12, 4)), 12 -> Mine(12, Offset(20, 3)), 14 -> Mine(14, Offset(3, 2))), 1)
      , CaribbeanState(CaribbeanContext(Map(), Map()), Map(0 -> Ship(0, Offset(5, 1), 4, 0, 100, 1), 5 -> Ship(5, Offset(17, 16), 1, 0, 100, 0), 1 -> Ship(1, Offset(5, 19), 2, 0, 100, 0), 2 -> Ship(2, Offset(12, 7), 5, 0, 100, 1), 3 -> Ship(3, Offset(12, 13), 1, 0, 100, 0), 4 -> Ship(4, Offset(17, 4), 5, 0, 100, 1)), Map(24 -> Barrel(24, Offset(10, 17), 19), 37 -> Barrel(37, Offset(10, 7), 17), 25 -> Barrel(25, Offset(9, 7), 14), 14 -> Barrel(14, Offset(11, 17), 14), 20 -> Barrel(20, Offset(3, 19), 11), 28 -> Barrel(28, Offset(12, 16), 20), 38 -> Barrel(38, Offset(10, 13), 17), 21 -> Barrel(21, Offset(9, 6), 10), 33 -> Barrel(33, Offset(13, 3), 13), 13 -> Barrel(13, Offset(11, 3), 14), 32 -> Barrel(32, Offset(2, 10), 13), 34 -> Barrel(34, Offset(13, 17), 13), 17 -> Barrel(17, Offset(15, 7), 12), 22 -> Barrel(22, Offset(9, 14), 10), 27 -> Barrel(27, Offset(12, 4), 20), 35 -> Barrel(35, Offset(14, 3), 19), 18 -> Barrel(18, Offset(15, 13), 12), 16 -> Barrel(16, Offset(10, 18), 11), 31 -> Barrel(31, Offset(8, 19), 16), 26 -> Barrel(26, Offset(9, 13), 14), 23 -> Barrel(23, Offset(10, 3), 19), 36 -> Barrel(36, Offset(14, 17), 19), 30 -> Barrel(30, Offset(8, 1), 16), 19 -> Barrel(19, Offset(3, 1), 11), 15 -> Barrel(15, Offset(10, 2), 11)), Map(), Map(6 -> Mine(6, Offset(11, 4)), 9 -> Mine(9, Offset(19, 2)), 11 -> Mine(11, Offset(16, 4))), 1)
      , CaribbeanState(CaribbeanContext(Map(), Map()), Map(0 -> Ship(0, Offset(4, 3), 4, 0, 100, 1), 5 -> Ship(5, Offset(21, 12), 1, 0, 100, 0), 1 -> Ship(1, Offset(4, 17), 2, 0, 100, 0), 2 -> Ship(2, Offset(11, 3), 1, 0, 100, 1), 3 -> Ship(3, Offset(11, 17), 5, 0, 100, 0), 4 -> Ship(4, Offset(21, 8), 5, 0, 100, 1)), Map(24 -> Barrel(24, Offset(14, 17), 20), 37 -> Barrel(37, Offset(13, 12), 17), 25 -> Barrel(25, Offset(4, 2), 12), 14 -> Barrel(14, Offset(20, 6), 20), 29 -> Barrel(29, Offset(6, 10), 12), 28 -> Barrel(28, Offset(18, 19), 13), 38 -> Barrel(38, Offset(21, 5), 14), 21 -> Barrel(21, Offset(5, 3), 19), 33 -> Barrel(33, Offset(5, 11), 12), 32 -> Barrel(32, Offset(5, 9), 12), 17 -> Barrel(17, Offset(15, 17), 11), 22 -> Barrel(22, Offset(5, 17), 19), 27 -> Barrel(27, Offset(18, 1), 13), 39 -> Barrel(39, Offset(21, 15), 14), 35 -> Barrel(35, Offset(5, 10), 12), 18 -> Barrel(18, Offset(8, 7), 12), 16 -> Barrel(16, Offset(15, 3), 11), 31 -> Barrel(31, Offset(4, 14), 13), 26 -> Barrel(26, Offset(4, 18), 12), 23 -> Barrel(23, Offset(14, 3), 20), 36 -> Barrel(36, Offset(13, 8), 17), 30 -> Barrel(30, Offset(4, 6), 13), 19 -> Barrel(19, Offset(8, 13), 12), 15 -> Barrel(15, Offset(20, 14), 20)), Map(), Map(8 -> Mine(8, Offset(18, 5)), 10 -> Mine(10, Offset(6, 4)), 12 -> Mine(12, Offset(7, 4))), 1)
      , CaribbeanState(CaribbeanContext(Map(), Map()), Map(0 -> Ship(0, Offset(6, 5), 5, 0, 100, 1), 2 -> Ship(2, Offset(12, 8), 1, 0, 100, 1), 1 -> Ship(1, Offset(6, 15), 1, 0, 100, 0), 3 -> Ship(3, Offset(12, 12), 5, 0, 100, 0)), Map(24 -> Barrel(24, Offset(2, 6), 12), 25 -> Barrel(25, Offset(2, 14), 12), 14 -> Barrel(14, Offset(15, 7), 13), 20 -> Barrel(20, Offset(6, 9), 14), 21 -> Barrel(21, Offset(6, 11), 14), 13 -> Barrel(13, Offset(16, 16), 13), 17 -> Barrel(17, Offset(10, 18), 13), 22 -> Barrel(22, Offset(12, 2), 13), 12 -> Barrel(12, Offset(16, 4), 13), 18 -> Barrel(18, Offset(11, 8), 10), 16 -> Barrel(16, Offset(10, 2), 13), 11 -> Barrel(11, Offset(18, 10), 11), 23 -> Barrel(23, Offset(12, 18), 13), 19 -> Barrel(19, Offset(11, 12), 10), 15 -> Barrel(15, Offset(15, 13), 13)), Map(), Map(5 -> Mine(5, Offset(13, 13)), 4 -> Mine(4, Offset(13, 7)), 6 -> Mine(6, Offset(16, 8)), 8 -> Mine(8, Offset(16, 6))), 1)
    )
    GameSimulator.evaluateOffline(
      games.flatMap(g => (0 until 5).map(_ => g)),
      CaribbeanArena,
      Vector(CaribbeanPlayer(CaribbeanContext.me, CaribbeanContext.other), BestCaribbeanPlayer(CaribbeanContext.other, CaribbeanContext.me)),
      200
    )
  }
}

object CaribbeanArenaDebug {
  def main(args: Array[String]): Unit = {
    val state = CaribbeanState(CaribbeanContext(Map(), Map()),
      Vector(
        Ship(0, Offset(4, 1), 4, 0, 100, 1),
        Ship(2, Offset(17, 6), 1, 0, 100, 1),
        Ship(1, Offset(4, 19), 2, 0, 100, 0),
        Ship(3, Offset(17, 14), 5, 0, 100, 0)
      ).map(e => e.id -> e).toMap,
      Vector(
        Barrel(12, Offset(2, 12), 18),
        Barrel(11, Offset(2, 8), 18),
        Barrel(14, Offset(6, 14), 19),
        Barrel(13, Offset(6, 6), 19),
        Barrel(16, Offset(6, 16), 11),
        Barrel(15, Offset(6, 4), 11),
        Barrel(18, Offset(15, 17), 17),
        Barrel(17, Offset(15, 3), 17),
        Barrel(21, Offset(7, 13), 15),
        Barrel(20, Offset(7, 7), 15),
        Barrel(23, Offset(2, 18), 15),
        Barrel(22, Offset(2, 2), 15),
        Barrel(25, Offset(4, 17), 10),
        Barrel(24, Offset(4, 3), 10),
        Barrel(27, Offset(6, 13), 10),
        Barrel(26, Offset(6, 7), 10),
        Barrel(28, Offset(13, 10), 10),
        Barrel(31, Offset(12, 13), 14),
        Barrel(30, Offset(12, 7), 14),
        Barrel(34, Offset(1, 15), 16),
        Barrel(33, Offset(1, 5), 16)).map(e => e.id -> e).toMap,
      Map.empty,
      Map(7 -> Mine(7, Offset(4, 5))), 1)
    val myPlayer = CaribbeanPlayer(1, 0)
    val otherPlayer = FixedCabribbeanPlayer(0, 1)
    (0 until 200).foreach(_ => {
      val after = GameSimulator.simulate(200, state, CaribbeanArena, Vector(myPlayer, otherPlayer))
      println(after)
    })
  }
}
import java.util.concurrent.atomic.AtomicInteger

object CaribbeanArena extends GameArena[CaribbeanState, CaribbeanAction] {
  val counter = new AtomicInteger(-1)
  override def next(state: CaribbeanState, actions: Vector[CaribbeanAction]): CaribbeanState = {
    val movedBalls = state.balls.mapValues(b => b.copy(land = b.land - 1))
    val shipsAfterDecreasedRum = state.ships.mapValues(s => s.copy(rums = s.rums - 1)).filter(_._2.rums > 0)
    val actionByShip = actions.map(a => a.shipId -> a).toMap
    val firedBalls = actionByShip.filter(_._2.isInstanceOf[Fire]).map {
      case (_, Fire(shipId, target)) =>
        val ship = state.ships(shipId)
        val targetCube = CaribbeanContext.toCube(target)
        val distance = ship.bow.distanceTo(targetCube)
        val travelTime = CollisionAnalysis.travelTime(distance)
        val id = counter.getAndDecrement()
        id -> Ball(id, target, ship.owner, travelTime)
      case _ => throw new IllegalStateException("Already filtered")
    }
    val shipsAfterSpeeding = shipsAfterDecreasedRum.mapValues(ship => {
      actionByShip(ship.id) match {
        case Faster(_) => ship.copy(speed = 2.min(ship.speed + 1))
        case Slower(_) => ship.copy(speed = 0.max(ship.speed - 1))
        case _ => ship
      }
    })
    val shipsAfterFirstMove = shipsAfterSpeeding.mapValues(s => moveShip(s, 1))
    val (shipsAfterFirstMoveImpact, barrelsAfterFirstMoveImpact, minesAfterFirstMoveImpact) =
      reactToShips(shipsAfterDecreasedRum, shipsAfterFirstMove, state.barrels, state.mines)
    val shipsAfterSecondMove = shipsAfterFirstMoveImpact.mapValues(s => moveShip(s, 2))
    val (shipsAfterSecondMoveImpact, barrelsAfterSecondMoveImpact, minesAfterSecondMoveImpact) =
      reactToShips(shipsAfterFirstMoveImpact, shipsAfterSecondMove,
        barrelsAfterFirstMoveImpact, minesAfterFirstMoveImpact)
    val shipsAfterRotation = shipsAfterSecondMoveImpact.mapValues(ship => {
      actionByShip(ship.id) match {
        case Port(_) => ship.copy(orientation = (ship.orientation + 1) % 6)
        case Starboard(_) => ship.copy(orientation = (ship.orientation + 5) % 6)
        case _ => ship
      }
    })
    val (shipsAfterRotationReact, barrelsAfterRotationReact, minesAfterRotationReact) =
      reactToShips(shipsAfterSecondMoveImpact, shipsAfterRotation,
        barrelsAfterSecondMoveImpact, minesAfterSecondMoveImpact)
    val (ballExplosions, remainingBalls) = movedBalls.partition(_._2.land == 0)
    val (mineExplosions, remainingMines) = minesAfterRotationReact.partition(e => ballExplosions.exists(_._2.cube == e._2.cube))
    val shipsAfterExplosion = shipsAfterRotationReact.mapValues(ship => {
      val dmgOnBow = ballExplosions.count(b => b._2.cube == ship.bow) * CaribbeanContext.lowBallDamage
      val dmgOnStern = ballExplosions.count(b => b._2.cube == ship.stern) * CaribbeanContext.lowBallDamage
      val dmgOnCenter = ballExplosions.count(b => b._2.cube == ship.center) * CaribbeanContext.highBallDamage
      val dmgFromMine = mineExplosions.count(m => m._2.cube.distanceTo(ship.center) == 1) * CaribbeanContext.lowMineDamage
      ship.copy(rums = ship.rums - dmgOnBow - dmgOnStern - dmgOnCenter - dmgFromMine)
    })
    state.copy(
      context = CaribbeanController.nextContext(state.context, state, actions),
      ships = shipsAfterExplosion.filter(_._2.rums > 0),
      barrels = barrelsAfterRotationReact,
      mines = remainingMines,
      balls = remainingBalls ++ firedBalls,
      turn = state.turn + 1
    )
  }
  def reactToShips(shipsBeforeAction: Map[Int, Ship],
                   shipsAfterAction: Map[Int, Ship],
                   barrels: Map[Int, Barrel],
                   mines: Map[Int, Mine]
                  ): (Map[Int, Ship], Map[Int, Barrel], Map[Int, Mine]) = {
    val shipsAfterActionReact = reactToShipsAction(shipsBeforeAction, shipsAfterAction)
    reactShipsImpacts(shipsAfterActionReact, barrels, mines)
  }
  def reactToShipsAction(shipsBeforeAction: Map[Int, Ship],
                         shipsAfterAction: Map[Int, Ship]): Map[Int, Ship] = {
    val collisions = shipCollisions(shipsAfterAction.values.toVector)
    shipsAfterAction.mapValues(ship => {
      if (collisions.contains(ship.id)) shipsBeforeAction(ship.id).copy(speed = 0) else ship
    })
  }
  def reactShipsImpacts(ships: Map[Int, Ship],
                        barrels: Map[Int, Barrel],
                        mines: Map[Int, Mine]
                       ): (Map[Int, Ship], Map[Int, Barrel], Map[Int, Mine]) = {
    val cubeToBarrel: Map[Cube, Barrel] = barrels.values.map(b => b.cube -> b).toMap
    val cubeToMine: Map[Cube, Mine] = mines.values.map(b => b.cube -> b).toMap
    val shipBarrelCollision: Iterable[(Ship, Barrel)] = findShipBarrelCollision(ships, cubeToBarrel)
    val shipsAfterShipBarrelCollision = shipBarrelCollision.foldLeft(ships) {
      case (updatedShips, (ship, barrel)) => updatedShips.updated(ship.id, ship.copy(rums =
        CaribbeanContext.maxRums.min(ship.rums + barrel.rums)))
    }
    val barrelsAfterShipMove = shipBarrelCollision.foldLeft(barrels) {
      case (remaining, (_, barrel)) => remaining - barrel.id
    }
    val shipMineCollision: Iterable[(Mine, Ship)] = findShipMineCollisions(ships, cubeToMine)
    val shipsAfterShipMineCollision = shipMineCollision.foldLeft(shipsAfterShipBarrelCollision) {
      case (updatedShips, (_, ship)) =>
        updatedShips.updated(ship.id, ship.copy(rums = ship.rums - CaribbeanContext.highMineDamage))
    }
    val minesAfterShipMove = shipMineCollision.foldLeft(mines) {
      case (remaining, (mine, _)) => remaining - mine.id
    }
    (shipsAfterShipMineCollision, barrelsAfterShipMove, minesAfterShipMove)
  }
  private def findShipMineCollisions(ships: Map[Int, Ship], cubeToMine: Map[Cube, Mine]) = {
    val shipMineCollision = for {
      ship <- ships.values
      shipZone <- ship.bowAndStern
      mine <- cubeToMine.get(shipZone)
    } yield (mine, ship)
    shipMineCollision
  }
  private def findShipBarrelCollision(ships: Map[Int, Ship], cubeToBarrel: Map[Cube, Barrel]) = {
    for {
      ship <- ships.values
      shipCube <- ship.bowAndStern
      barrel <- cubeToBarrel.get(shipCube)
    } yield (ship, barrel)
  }
  def moveShip(ship: Ship, speed: Int): Ship = {
    if (ship.speed >= speed) {
      if (!CaribbeanContext.inside(ship.bow.toOffset)) {
        ship.copy(speed = 0)
      } else {
        ship.copy(position = ship.bow.toOffset)
      }
    } else {
      ship
    }
  }
  def shipCollisions(ships: Vector[Ship]): Set[Int] = {
    ships.combinations(2).foldLeft(Set[Int]()) {
      case (collisions, shipPair) =>
        if (collided(shipPair.head, shipPair.last)) {
          collisions + shipPair.head.id + shipPair.last.id
        } else collisions
    }
  }
  def collided(one: Ship, other: Ship): Boolean = {
    one.center.distanceTo(other.center) < 3 && one.zone.exists(other.zone.contains)
  }
  override def judge(state: CaribbeanState): GameResult = {
    val myShips = state.shipsOf(CaribbeanContext.me)
    val otherShips = state.shipsOf(CaribbeanContext.other)
    if (myShips.isEmpty && otherShips.isEmpty) {
      Draw
    } else if (myShips.isEmpty) {
      LossKO
    } else if (otherShips.isEmpty) {
      WinKO
    } else {
      val myTotalRums = myShips.map(_.rums).sum
      val otherTotalRums = otherShips.map(_.rums).sum
      if (myTotalRums > otherTotalRums) {
        WinTech
      } else if (myTotalRums == otherTotalRums) {
        LossTech
      } else {
        Draw
      }
    }
  }
}
import scala.concurrent.duration.Duration

case class CaribbeanPlayer(me: Int, other: Int,
                           stopper: Stopper = new Chronometer(Duration(47, TimeUnit.MILLISECONDS))
                          ) extends GamePlayer[CaribbeanState, CaribbeanAction] {
  override def reactTo(state: CaribbeanState): Vector[CaribbeanAction] = {
    val myShips = state.shipsOf(me)
    val result = if (state.barrels.isEmpty && myShips.size > 1) {
      val weakest = myShips.minBy(_.rums)
      val distance = myShips.filter(_.id != weakest.id).map(s =>
        CollisionAnalysis.collisionTime(s, weakest.center)).min
      if (distance <= 3 && weakest.rums <= 50 && weakest.speed < 2 &&
        !state.balls.exists(e => e._2.land <= 2 && e._2.cube == weakest.center)) {
        myShips.map(s => if (s.id == weakest.id) Fire(s.id, CollisionAnalysis.hitMyself(weakest).toOffset) else Wait(s.id))
      } else {
        simulate(state)
      }
    } else {
      simulate(state)
    }
    val otherShips = state.shipsOf(other)
    val mineable = myShips.find(ship => otherShips.exists(os => CollisionAnalysis.canMine(ship, os)))
    if (mineable.isEmpty) result else {
      val ship = mineable.get
      result.map(r => if (r.shipId == ship.id) MineAction(ship.id) else r)
    }
  }
  private def simulate(state: CaribbeanState) = {
    val muToLambda = new MuPlusLambda(1, 3, stopper)
    val solution = muToLambda.search(CaribbeanProblem(me, other, FixedCabribbeanPlayer(other, me), state))
    solution.toActions
  }
}
case class CaribbeanProblem(me: Int,
                            other: Int,
                            otherPlayer: GamePlayer[CaribbeanState, CaribbeanAction],
                            state: CaribbeanState) extends Problem[CaribbeanSolution] {
  private val convolution = new BoundedVectorConvolution(1.0, 0, 10)
  private val roundsToSimulate = 3
  val rounds: Range = 0 until roundsToSimulate
  val actionLength: Int = state.shipsOf(me).size
  val chromosome: Range = 0 until (roundsToSimulate * actionLength)
  override def randomSolution(): CaribbeanSolution = {
    CaribbeanSolution(this, chromosome.map(_ => NoiseGenerators.uniform(5, 5).apply()).toVector)
  }
  override def tweakSolution(solution: CaribbeanSolution): CaribbeanSolution = {
    solution.copy(actions = convolution.tweak(solution.actions,
      NoiseGenerators.gaussian(mean = 0, stdDerivation = 7)))
  }
}
case class CaribbeanSolution(problem: CaribbeanProblem,
                             actions: Vector[Double]) extends Solution {
  val quality: Double = {
    val simulatedState = targetState()
    val otherShips = simulatedState.shipsOf(problem.other)
    val myShips = simulatedState.shipsOf(problem.me)
    val otherScore = 0.001 * otherShips.map(ship => {
      ship.rums
    }).sum
    if (problem.state.barrels.isEmpty && myShips.size > 1 && myShips.exists(_.rums < 53)) {
      myShips.map(ship => {
        val shipValues = myShips.map(ms => {
          ms.rums * Math.pow(0.5, ms.center.distanceTo(ship.center))
        }).sum
        ship.rums + 0.001 * shipValues
      }).sum - otherScore
    } else {
      myShips.map(ship => {
        val barrelValues = simulatedState.barrels.values
          .map(b => b.rums * Math.pow(0.95, CollisionAnalysis.collisionTime(ship, b.cube))).sum
        val freeHex = CaribbeanContext.reachable(ship.center).size
        ship.rums + 0.001 * barrelValues + 0.0001 * freeHex
      }).sum - otherScore
    }
  }
  def targetState(): CaribbeanState = {
    problem.rounds.foldLeft(problem.state)((s, r) => {
      val shipActions = actions.slice(r * problem.actionLength, (r + 1) * problem.actionLength)
      val adapted = adapt(s, shipActions)
      CaribbeanArena.next(s, adapted ++ problem.otherPlayer.reactTo(s))
    })
  }
  def adapt(state: CaribbeanState, shipActions: Vector[Double]): Vector[CaribbeanAction] = {
    val myShips = state.shipsOf(problem.me)
    val otherShips = state.shipsOf(problem.other)
    myShips.indices.map(i => {
      val ship = myShips(i)
      val x = shipActions(i)
      x match {
        case _ if x > 9 || x < 1 => Port(ship.id)
        case _ if x > 8 || x < 2 => Starboard(ship.id)
        case _ if x > 7 || x < 3 => if (ship.speed == 2) Port(ship.id) else Faster(ship.id)
        case _ if x > 6 || x < 4 => if (ship.speed == 0) Starboard(ship.id) else Slower(ship.id)
        case y =>
          val targets: Vector[Cube] = otherShips
            .filter(s => s.nextNextCenter.distanceTo(ship.bow) <= CaribbeanContext.fireMaxDistance)
            .map(_.nextNextCenter)
          if (targets.isEmpty) {
            if (y > 5) Port(ship.id) else Starboard(ship.id)
          } else {
            val target = targets((y * 100).toInt % targets.size)
            Fire(ship.id, target.toOffset)
          }
      }
    }).toVector
  }
  def toActions: Vector[CaribbeanAction] = {
    adapt(problem.state, actions.take(problem.actionLength))
  }
}
import scala.io.StdIn

object Player {
  val gameLoop = new GameLoop(CaribbeanController, CaribbeanPlayer(1, 0))
  val predictableGameLoop = new PredictableGameLoop(CaribbeanController,
    CaribbeanPlayer(1, 0),
    FixedCabribbeanPlayer(0, 1),
    CaribbeanArena)
  def main(args: Array[String]): Unit = {
    gameLoop.run()
  }
}