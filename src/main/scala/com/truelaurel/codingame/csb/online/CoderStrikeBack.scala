import com.truelaurel.codingame.csb.head.StrikeBackPlayer
import com.truelaurel.codingame.csb.online.StrikeBackGameReader
import com.truelaurel.codingame.engine.TurnBasedPlayer

object Player {

  private val turnBasedPlayer = new TurnBasedPlayer(
    StrikeBackGameReader.reader,
    StrikeBackPlayer(Vector(0, 1), Vector(2, 3))
  )

  def main(args: Array[String]): Unit = {
    turnBasedPlayer.run()
  }

}