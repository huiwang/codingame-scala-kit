package com.tyrcho.ghostinthecell

sealed trait Entity

case class Factory(id: Int, mine: Option[Boolean], cyborgs: Int, maxProduction: Int, disabled: Int = 0) extends Entity {
  override def toString =
    s"F $id $cyborgs defenders - produces $production $mine" +
      (if (disabled > 0) s" disabled for $disabled" else "")

  val production = if (disabled > 0) 0 else maxProduction

  def reverse = copy(mine = mine.map(!_))

  def nextTurn = copy(disabled = disabled - 1 max 0)

  def upgrade = copy(cyborgs = cyborgs - 10, maxProduction = maxProduction + 1)

  def canSendToCenter(incomingDelta: Int): Option[Int] =
    if (id == 0) None
    else {
      val needed = if (maxProduction == 3) 0 else 10
      val available = cyborgs + incomingDelta - needed max 0
      if (available > 0) Some(available / 2 max 1) else None
    }

  def canIncrease = cyborgs >= 10 && production < 3

  //lower is better
  def score = -production * 100 /
    (1 + cyborgs * (if (mine == None) 2 else 1))

  def receiveBomb: Factory = {
    val dmg = (cyborgs / 2) max 10 min cyborgs
    copy(cyborgs = cyborgs - dmg, disabled = 5)
  }

  def receiveCyborgs(count: Int): Factory = {
    mine match {
      case Some(true)  => copy(cyborgs = cyborgs + count)
      case Some(false) => copy(cyborgs = cyborgs - count)
      case None =>
        if (count.abs <= cyborgs) copy(cyborgs = cyborgs - count.abs)
        else copy(cyborgs = count.abs - cyborgs, mine = Some(count > 0))
    }
  }.checkSide

  private def checkSide =
    if (cyborgs < 0) copy(mine = mine.map(!_), cyborgs = -cyborgs)
    else this
}

case class Bomb(id: Int, mine: Boolean, from: Int, to: Int, timeLeft: Int, time: Int = 1) extends Entity {
  def nextTurn = copy(time = time + 1, timeLeft = timeLeft - 1)

  def reverse = copy(mine = !mine)

}

case class Troop(id: Int, mine: Boolean, from: Int, to: Int, cyborgs: Int, timeLeft: Int, target: Option[Int] = None) extends Entity {
  def isStayMove = from == to

  override def toString =
    (if (mine) "my" else "enemy") +
      s" troop of $cyborgs cyborgs arriving in $timeLeft turns at $to (left $from with intent $target)"

  def reverse = copy(mine = !mine)
}
