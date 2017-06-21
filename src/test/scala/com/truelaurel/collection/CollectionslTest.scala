package com.truelaurel.collection

import com.truelaurel.collection.Collectionsl._
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by Wei on 14/06/2017.
  */
class CollectionslTest extends FlatSpec with Matchers {

  behavior of "Adjust Function"

  it should "Update the assigned key with the assigned function" in {
    adjust(Map(1 -> 2))(1)(_ + 1)(1) should equal(3)
  }

}
