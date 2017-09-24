package ru.spbau.jvm.scala.homework03.database

import akka.persistence.PersistentActor

import scala.collection.mutable

class LabyrinthActor extends PersistentActor {

  import LabyrinthActor._

  val map: mutable.HashMap[Long, Labyrinth] =
    mutable.HashMap.empty

  def receiveEvent(event: Event): Unit = {
    event match {
      case Start(id, width, height) => map.update(id, new Labyrinth(width, height))
      case Finish(id) => map.remove(id)
    }
  }

  override def receiveRecover: Receive = {
    case evt: Event => receiveEvent(evt)
  }

  override def receiveCommand: Receive = {
    case evt: Event => persist(evt)(receiveEvent)
    case GetPosition(id) =>
      sender ! Position(map.get(id).map(_.currentPosition).getOrElse((-1, -1)))
    case Go(id, direction, steps) =>
      sender ! GoResult(map.get(id).map(_.goTo(direction, steps)).getOrElse(NoLabyrinth))
  }

  override def persistenceId = "au-labyrinth-database"
}

object LabyrinthActor {

  //events
  trait Event

  case class Start(id: Long, width: Int, height: Int) extends Event

  case class Finish(id: Long) extends Event

  //queries
  case class GetPosition(id: Long)

  case class Position(position: (Int, Int))

  case class Go(id: Long, direction: String, steps: Int)

  case class GoResult(result: MoveResult)
}
