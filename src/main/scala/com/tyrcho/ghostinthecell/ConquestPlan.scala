package com.tyrcho.ghostinthecell

case class ConquestPlans(plans: Vector[ConquestPlan], achieved: Int) {
  val failed: Boolean = plans.isEmpty
  val cyborgs: Int = plans.map(_.cyborgs).sum

  def delayed: Boolean = plans.forall(_.delay > 0)
}

case class ConquestPlan(from: Int, to: Int, cyborgs: Int, delay: Int)
