package ru.spbau.jvm.scala.homework03.database.labyrinth

trait Cell

case object EmptyCell extends Cell

case object WallCell extends Cell

case object ExitCell extends Cell