
case class BestStrikeBackPlayer(player: Int) extends GamePlayer[StrikeBackState, StrikeBackAction] {
  override def reactTo(state: StrikeBackState): Vector[StrikeBackAction] = {
    state.podsOf(player).map(pod => Thrust(pod.id, state.checkPoint(pod.goal), 100))
  }
}
case class Disk(p: Vectorl, v: Vectorl, r: Double, m: Double = 1.0) {
}
object DiskMover {
  def move(disk: Disk, time: Double): Disk = {
    disk.copy(p = disk.p + (disk.v * time))
  }
}
object Collision {
  def collideTime(d1: Disk, d2: Disk): Option[Double] = {
    collideTime(d1.p, d1.v, d1.r, d2.p, d2.v, d2.r)
  }
  def collideTime(p1: Vectorl, v1: Vectorl, r1: Double,
                  p2: Vectorl, v2: Vectorl, r2: Double): Option[Double] = {
    val dr = p2 - p1
    val dv = v2 - v1
    val dvdr = dv.dotProduct(dr)
    if (dvdr > 0) return None
    val dvdv = dv.dotProduct(dv)
    if (dvdv == 0) return None
    val drdr = dr.dotProduct(dr)
    val sigma = r1 + r2
    val d = (dvdr * dvdr) - dvdv * (drdr - sigma * sigma)
    if (d < 0) return None
    Some(-(dvdr + Math.sqrt(d)) / dvdv)
  }
  def bounceOff(p1: Vectorl, v1: Vectorl, m1: Double,
                p2: Vectorl, v2: Vectorl, m2: Double): (Vectorl, Vectorl) = {
    val impulse = computeImpulse(p1, v1, m1, p2, v2, m2)
    bouncedSpeed(v1, m1, v2, m2, impulse)
  }
  private def bouncedSpeed(v1: Vectorl, m1: Double, v2: Vectorl, m2: Double, impulse: Vectorl) = {
    (v1 + impulse / m1, v2 - impulse / m2)
  }
  def bounceOffWithMinimumImpulse(p1: Vectorl, v1: Vectorl, m1: Double,
                                  p2: Vectorl, v2: Vectorl, m2: Double,
                                  minImpulse: Double): (Vectorl, Vectorl) = {
    val impulse = computeImpulse(p1, v1, m1, p2, v2, m2)
    val adjusted = impulse.norm * (impulse.mag * 0.5 + minImpulse).max(impulse.mag)
    bouncedSpeed(v1, m1, v2, m2, adjusted)
  }
  private def computeImpulse(p1: Vectorl, v1: Vectorl, m1: Double, p2: Vectorl, v2: Vectorl, m2: Double) = {
    val dr = p2 - p1
    val dv = v2 - v1
    val drdr = dr.dotProduct(dr)
    val dvdr = dr.dotProduct(dv)
    val massCoefficient = (m1 + m2) / (m1 * m2)
    val impulse = dr * 2.0 * dvdr / (massCoefficient * drdr)
    impulse
  }
  def bounceOff(d1: Disk, d2: Disk): (Disk, Disk) = {
    val (v1, v2) = bounceOff(d1.p, d1.v, d1.m, d2.p, d2.v, d2.m)
    (d1.copy(v = v1), d2.copy(v = v2))
  }
  def move(disk: Disk, time: Double): Disk = DiskMover.move(disk, time)
}
object StrikeBackCollisionSimulation {
  def simulate(checkPoints: Vector[CheckPoint], pods: Vector[Pod], duration: Double): Vector[Pod] = {
    val podCpCollisions = for {
      pod <- pods
      cp <- checkPoints
      if pod.goal % checkPoints.size == cp.id
      time <- Collision.collideTime(pod.position, pod.speed, pod.radius, cp.position, cp.speed, cp.radius)
      if time < duration
    } yield (pod, cp, time)
    val podPodCollisions = for {
      pod1 <- pods
      pod2 <- pods
      if pod1.id < pod2.id
      time <- Collision.collideTime(pod1.position, pod1.speed, pod1.radius, pod2.position, pod2.speed, pod2.radius)
      if time < duration
    } yield (pod1, pod2, time)
    CGLogger.debug(podCpCollisions)
    CGLogger.debug(podPodCollisions)
    if (podCpCollisions.isEmpty && podPodCollisions.isEmpty) {
      pods.map(movePod(_, duration))
    } else if (podCpCollisions.isEmpty) {
      val (pod1, pod2, time) = podPodCollisions.minBy(_._3)
      simulate(checkPoints, movePodPodCollision(pods, pod1, pod2, time), duration - time)
    } else if (podPodCollisions.isEmpty) {
      val (pod, cp, time) = podCpCollisions.minBy(_._3)
      simulate(checkPoints, movePodCpCollision(pods, pod, time), duration - time)
    } else {
      val (pod1, pod2, timePodPod) = podPodCollisions.minBy(_._3)
      val (pod, cp, timePodCp) = podCpCollisions.minBy(_._3)
      if (timePodPod < timePodCp) {
        simulate(checkPoints, movePodPodCollision(pods, pod1, pod2, timePodPod), duration - timePodPod)
      } else {
        simulate(checkPoints, movePodCpCollision(pods, pod, timePodCp), duration - timePodCp)
      }
    }
  }
  private def movePodCpCollision(pods: Vector[Pod], pod: Pod, time: Double) = {
    pods.map(p => if (p.id == pod.id) movePodAndGoal(p, time) else movePod(p, time))
  }
  private def movePodPodCollision(pods: Vector[Pod], pod1: Pod, pod2: Pod, time: Double) = {
    val (bouncedPod1, bouncedPod2) = bounceOff(pod1, pod2, time)
    pods.map(p => p.id match {
      case pod1.id => bouncedPod1
      case pod2.id => bouncedPod2
      case _ => movePod(p, time)
    })
  }
  private def bounceOff(pod1: Pod, pod2: Pod, time: Double) = {
    val movedPod1 = movePod(pod1, time)
    val movedPod2 = movePod(pod2, time)
    val (speed1, speed2) = Collision.bounceOffWithMinimumImpulse(
      movedPod1.position, movedPod1.speed, movedPod1.mass,
      movedPod2.position, movedPod2.speed, movedPod2.mass,
      120.0
    )
    val bouncedPod1 = movedPod1.copy(speed = speed1)
    val bouncedPod2 = movedPod2.copy(speed = speed2)
    (bouncedPod1, bouncedPod2)
  }
  private def movePod(pod: Pod, duration: Double) = {
    pod.copy(position = pod.position + (pod.speed * duration))
  }
  private def movePodAndGoal(pod: Pod, duration: Double) = {
    pod.copy(position = pod.position + (pod.speed * duration), goal = pod.goal + 1)
  }
}

object StrikeBackArenaDebug {
  def main(args: Array[String]): Unit = {
    val state = StrikeBackState(Vector(CheckPoint(0,Vectorl(4069.0,4654.0)), CheckPoint(1,Vectorl(13046.0,1911.0)), CheckPoint(2,Vectorl(6539.0,7863.0))),Vector(Pod(0,Vectorl(5515.0,3787.0),Vectorl(340.0,-83.0),Vectorl(0.9702957262759965,-0.24192189559966787),1), Pod(1,Vectorl(5756.0,4573.0),Vectorl(329.0,-120.0),Vectorl(0.9396926207859081,-0.34202014332566943),1), Pod(2,Vectorl(5255.0,2996.0),Vectorl(347.0,-47.0),Vectorl(0.9902680687415703,-0.13917310096006588),1), Pod(3,Vectorl(5982.0,5370.0),Vectorl(316.0,-153.0),Vectorl(0.8987940462991668,-0.4383711467890778),1)))
    val me = BestStrikeBackPlayer(StrikeBackContext.me)
    val other = BestStrikeBackPlayer(StrikeBackContext.other)
    val predicted = GameSimulator.singleTurn(state, StrikeBackArena, Vector(me, other))
    println(state)
    println(predicted)
  }
}

object StrikeBackArena extends GameArena[StrikeBackState, StrikeBackAction] {
  override def next(state: StrikeBackState, actions: Vector[StrikeBackAction]): StrikeBackState = {
    val steeredPods = for {
      action <- actions
      pod = state.pods(action.id)
    } yield action match {
      case Thrust(_, target, thrust) =>
        val pivoted = pod.angle.pivotTo(target - pod.position, 18.0)
        pod.copy(speed = pod.speed + pivoted * thrust)
      case AngleThrust(_, _, angle, rotate, thrust) =>
        val rotated = angle.rotateInDegree(rotate)
        pod.copy(speed = pod.speed + rotated * thrust, angle = rotated)
      case _ => pod
    }
    val movedPods = StrikeBackCollisionSimulation.simulate(state.checkPoints, steeredPods, 1.0)
    val slowed = movedPods.map(p => p.copy(speed = p.speed * .85))
    StrikeBackState(state.checkPoints, slowed)
  }
  override def judge(state: StrikeBackState): GameResult = {
    Draw
  }
}

trait Solution {
  def quality() : Double
}

trait Problem[S <: Solution] {
  def randomSolution(): S
  def tweakSolution(solution: S): S
}

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

object StrikeBackPlayerDebug {
  def main(args: Array[String]): Unit = {
    val state = StrikeBackState(Vector(CheckPoint(0, Vectorl(4069.0, 4654.0)), CheckPoint(1, Vectorl(13046.0, 1911.0)), CheckPoint(2, Vectorl(6539.0, 7863.0))), Vector(Pod(0, Vectorl(3923.0, 4176.0), Vectorl(0.0, 0.0), Vectorl(0.970535462692705, -0.24095832763334177), 1, 1.0), Pod(1, Vectorl(4215.0, 5132.0), Vectorl(0.0, 0.0), Vectorl(0.9394606319681904, -0.34265685602644563), 1, 1.0), Pod(2, Vectorl(3631.0, 3219.0), Vectorl(0.0, 0.0), Vectorl(0.9904870959429274, -0.1376056422191555), 1, 1.0), Pod(3, Vectorl(4507.0, 6089.0), Vectorl(0.0, 0.0), Vectorl(0.8982441135466412, -0.43949688563038614), 1, 1.0)))
    val player = StrikeBackPlayer(StrikeBackContext.me, StrikeBackContext.other)
    val actions = player.reactTo(state)
    println(actions)
  }
}
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration

case class StrikeBackPlayer(me: Int, other: Int) extends GamePlayer[StrikeBackState, StrikeBackAction] {
  override def reactTo(state: StrikeBackState): Vector[StrikeBackAction] = {
    val muToLambda = new MuPlusLambda(20, 100, new Chronometer(Duration(90, TimeUnit.MILLISECONDS)))
    val solution = muToLambda.search(StrikeBackProblem(me, other, BestStrikeBackPlayer(StrikeBackContext.other), state))
    solution.adapt(state, solution.actions.take(2))
  }
}
case class StrikeBackProblem(me: Int,
                             other: Int,
                             otherPlayer: GamePlayer[StrikeBackState, StrikeBackAction],
                             state: StrikeBackState) extends Problem[StrikeBackSolution] {
  private val convolution = new BoundedVectorConvolution(0.3, -180, +180)
  val chromosome: Range = 0 until 12
  val rounds: Range = 0 until 6
  override def randomSolution(): StrikeBackSolution = {
    StrikeBackSolution(this, chromosome.map(_ => NoiseGenerators.uniform(180).apply()).toVector)
  }
  override def tweakSolution(solution: StrikeBackSolution): StrikeBackSolution = {
    solution.copy(actions = convolution.tweak(solution.actions, NoiseGenerators.uniform(180)))
  }
}
case class StrikeBackSolution(problem: StrikeBackProblem,
                              actions: Vector[Double]) extends Solution {
  lazy val quality: Double = {
    targetState.podsOf(problem.me).map(pod => {
      pod.goal * 10000000.0 - (targetState.checkPoint(pod.goal) - pod.position).mag
    }).max
  }
  lazy val targetState: StrikeBackState = {
    problem.rounds.foldLeft(problem.state)((s, r) => {
      val podActions = actions.slice(r * 2, r * 2 + 2)
      val adapted = adapt(s, podActions)
      StrikeBackArena.next(s, adapted ++ problem.otherPlayer.reactTo(s))
    })
  }
  def adapt(s: StrikeBackState, podActions: Vector[Double]): Vector[AngleThrust] = {
    s.podsOf(problem.me).map(p => AngleThrust(p.id, p.position, p.angle, actions.head / 10, (actions.last + 180) / 1.8))
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
object Vectorls {
  val origin = Vectorl(0, 0)
  val axisX = Vectorl(1, 0)
  val axisY = Vectorl(0, 1)
}
case class Vectorl(x: Double, y: Double) {
  lazy val mag2: Double = x * x + y * y
  lazy val mag: Double = Math.sqrt(mag2)
  lazy val norm: Vectorl = if (mag > 0) this * (1.0 / mag) else Vectorl(0, 0)
  def dotProduct(that: Vectorl): Double = x * that.x + y * that.y
  def *(factor: Double): Vectorl = Vectorl(x * factor, y * factor)
  def /(factor : Double): Vectorl = this * (1.0 / factor)
  def +(that: Vectorl): Vectorl = Vectorl(x + that.x, y + that.y)
  def -(that: Vectorl): Vectorl = Vectorl(x - that.x, y - that.y)
  def perp = Vectorl(-y, x)
  def pivotTo(desired: Vectorl, maxDegree: Double): Vectorl = {
    if (mag2 == 0 || angleInDegreeBetween(desired) <= maxDegree) {
      desired.norm
    } else {
      if (this.perDotProduct(desired) > 0) {
        rotateInDegree(maxDegree).norm
      } else {
        rotateInDegree(-maxDegree).norm
      }
    }
  }
  def rotateInDegree(degree: Double): Vectorl = rotateInRadian(Math.toRadians(degree))
  def rotateInRadian(radians: Double): Vectorl = {
    val rotated = angleInRadian + radians
    Vectorl(Math.cos(rotated), Math.sin(rotated)) * mag
  }
  def angleInRadianBetween(other: Vectorl): Double = {
    val result = this.dotProduct(other) / (this.mag * other.mag)
    if (result >= 1.0) 0 else Math.acos(result)
  }
  def angleInDegreeBetween(other: Vectorl): Double = {
    Math.toDegrees(angleInRadianBetween(other))
  }
  private def angleInDegree: Double = Math.toDegrees(angleInRadian)
  private def angleInRadian: Double = Math.atan2(y, x)
  def perDotProduct(that: Vectorl): Double = perp.dotProduct(that)
  def between(v1: Vectorl, v2: Vectorl): Boolean = {
    this.perDotProduct(v1) * this.perDotProduct(v2) < 0
  }
  def truncate = Vectorl(x.toInt, y.toInt)
  def round = Vectorl(Mathl.halfUp(x), Mathl.halfUp(y))
  override def equals(o: Any): Boolean = o match {
    case that: Vectorl => Mathl.almostEqual(x, that.x) && Mathl.almostEqual(y, that.y)
    case _ => false
  }
}
case class StrikeBackContext(checkPoints: Vector[CheckPoint])
object StrikeBackContext {
  val podCount = 4
  val podIndices: Vector[Int] = (0 until podCount).toVector
  val me = 0
  val other = 1
}
case class Pod(id : Int, position: Vectorl, speed: Vectorl, angle: Vectorl, goal: Int, mass : Double = 1.0) {
  val radius = 400
}
case class CheckPoint(id : Int, position: Vectorl) {
  val radius = 200
  val speed: Vectorl = Vectorls.origin
}
case class StrikeBackState(checkPoints: Vector[CheckPoint],
                           pods: Vector[Pod]) {
  def podsOf(playerId: Int): Vector[Pod] = {
    if (playerId == 0) {
      pods.take(2)
    } else {
      pods.takeRight(2)
    }
  }
  def checkPoint(goal : Int) : Vectorl = checkPoints(goal % checkPoints.size).position
}
trait StrikeBackAction {
  def id: Int
}
sealed case class Thrust(id: Int, target: Vectorl, thrust: Int) extends StrikeBackAction {
  override def toString: String = s"${target.x.toInt} ${target.y.toInt} $thrust"
}
sealed case class AngleThrust(id: Int, position: Vectorl, angle: Vectorl, rotate: Double, thrust: Double) extends StrikeBackAction {
  override def toString: String = Thrust(id, position + angle.rotateInDegree(rotate) * 100000, thrust.toInt).toString
}
sealed case class Shield(id: Int) extends StrikeBackAction {
  override def toString: String = "SHIELD"
}
sealed case class Boost(id: Int) extends StrikeBackAction {
  override def toString: String = "BOOST"
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
import scala.io.StdIn
object StrikeBackController extends GameController[StrikeBackContext, StrikeBackState, StrikeBackAction] {
  override def readContext: StrikeBackContext = {
    val laps = StdIn.readInt()
    val checkpointCount = StdIn.readInt()
    val checkPoints = 0.until(checkpointCount).map(i => {
      val Array(x, y) = StdIn.readLine().split(" ").map(_.toInt)
      CheckPoint(i, Vectorl(x, y))
    }).toVector
    StrikeBackContext(checkPoints)
  }
  override def readState(turn: Int, context: StrikeBackContext): StrikeBackState = {
    val pods = StrikeBackContext.podIndices.map(i => {
      val Array(x, y, vx, vy, angle, goal) = StdIn.readLine().split(" ").map(_.toInt)
      val angleVec = if (angle == -1) {
        (context.checkPoints(goal).position - Vectorl(x, y)).norm
      } else {
        Vectorls.axisX.rotateInDegree(angle)
      }
      Pod(i, Vectorl(x, y), Vectorl(vx, vy), angleVec, goal)
    })
    StrikeBackState(context.checkPoints, pods)
  }
  override def nextContext(context: StrikeBackContext,
                           state: StrikeBackState,
                           actions: Vector[StrikeBackAction]): StrikeBackContext = context
}
object Player {
  private val gameLoop = new GameLoop(
    StrikeBackController,
    StrikeBackPlayer(StrikeBackContext.me, StrikeBackContext.other),
    turns = 800
  )
  private val predictable = new PredictableGameLoop(
    StrikeBackController,
    BestStrikeBackPlayer(StrikeBackContext.me),
    BestStrikeBackPlayer(StrikeBackContext.other),
    StrikeBackArena
  )
  def main(args: Array[String]): Unit = {
    gameLoop.run()
  }
}