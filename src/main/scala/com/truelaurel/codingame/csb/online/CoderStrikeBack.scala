import com.truelaurel.codingame.csb.best.BestStrikeBackPlayer
import com.truelaurel.codingame.csb.head.StrikeBackPlayer
import com.truelaurel.codingame.csb.offline.StrikeBackArena
import com.truelaurel.codingame.csb.online.StrikeBackGameReader
import com.truelaurel.codingame.engine.{ArenaAwarePlayer, TurnBasedPlayer}

object Player {
  private val arenaAwarePlayer = new ArenaAwarePlayer(
    StrikeBackGameReader.reader,
    BestStrikeBackPlayer(Vector(0, 1)),
    BestStrikeBackPlayer(Vector(2, 3)),
    StrikeBackArena
  )

  private val turnBasedPlayer = new TurnBasedPlayer(
    StrikeBackGameReader.reader,
    StrikeBackPlayer(Vector(0, 1), Vector(2, 3))
  )

  def main(args: Array[String]): Unit = {
    turnBasedPlayer.run()
  }

}