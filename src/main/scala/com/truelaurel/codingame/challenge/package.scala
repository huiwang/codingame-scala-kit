package com.truelaurel.codingame

package object challenge {

  implicit val reader: ConsoleReader = scala.io.StdIn
  implicit val writer: ConsoleWriter = scala.Console


  type ConsoleReader = {
    def readLine(): String

    def readInt(): Int
  }

  type ConsoleWriter = {
    def println(a: Any): Unit
  }


}
