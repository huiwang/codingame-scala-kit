import com.truelaurel.codingame.csb.arena.StrikeBackArena
import com.truelaurel.codingame.csb.best.BestStrikeBackPlayer
import com.truelaurel.codingame.csb.head.StrikeBackPlayer
import com.truelaurel.codingame.csb.io.StrikeBackController
import com.truelaurel.codingame.csb.model.StrikeBackContext
import com.truelaurel.codingame.engine.{GameLoop, PredictableGameLoop}

object Player {

  private val gameLoop = new GameLoop(
    StrikeBackController,
    StrikeBackPlayer(Vector(0, 1), Vector(2, 3))
  )

  private val predictable = new PredictableGameLoop(
    StrikeBackController,
    BestStrikeBackPlayer(StrikeBackContext.me),
    BestStrikeBackPlayer(StrikeBackContext.other),
    StrikeBackArena
  )

  def main(args: Array[String]): Unit = {
    predictable.run()
  }

}