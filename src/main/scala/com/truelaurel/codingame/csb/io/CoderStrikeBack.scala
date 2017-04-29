import com.truelaurel.codingame.csb.head.StrikeBackPlayer
import com.truelaurel.codingame.csb.io.StrikeBackController
import com.truelaurel.codingame.engine.GameLoop

object Player {

  private val gameLoop = new GameLoop(
    StrikeBackController,
    StrikeBackPlayer(Vector(0, 1), Vector(2, 3))
  )

  def main(args: Array[String]): Unit = {
    gameLoop.run()
  }

}