package com.truelaurel.codingame.challenge

sealed trait GameResult


case object Draw extends GameResult

case object WinKO extends GameResult

case object LossKO extends GameResult

case object WinTech extends GameResult

case object LossTech extends GameResult