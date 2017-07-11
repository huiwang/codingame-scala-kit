package com.truelaurel.samplegames.stirfry

import org.scalatest.{FlatSpec, Matchers}

class CardTest extends FlatSpec with Matchers {

  "Shrimps well done" should "score 22" in {
    Card.score(Seq(Shrimp, Soja, Ginger, Onion, Noodles)) shouldBe 22
  }

  "best meal" should "score 24" in {
    val best = Card.all.toSeq.combinations(5).filter(_.contains(Noodles)).maxBy(Card.score)
    println(best)
    Card.score(best) shouldBe 24
  }

  "best veg meal" should "score 11" in {
    val best = Card.all.toSeq.filter(!_.meat).combinations(5).maxBy(Card.score)
    println(best)
    Card.score(best) shouldBe 11
  }

}
