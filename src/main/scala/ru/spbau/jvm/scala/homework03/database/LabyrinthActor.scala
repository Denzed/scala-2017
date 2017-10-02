package ru.spbau.jvm.scala.homework03.database

import akka.persistence.PersistentActor
import ru.spbau.jvm.scala.homework03.database.labyrinth._

import scala.collection.mutable

class LabyrinthActor extends PersistentActor {

  import LabyrinthActor._

  val map: mutable.HashMap[Long, Labyrinth] =
    mutable.HashMap.empty

  def receiveEvent(event: Event): Unit = {
    event match {
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
    case MakeLabyrinth(id, width, height) =>
      sender ! MakeLabyrinthResult({
        val generated: Option[Labyrinth] = Labyrinth(width, height)
        if (generated.nonEmpty) {
          map.update(id, generated.get)
          GenerationSuccessful
        } else
          GenerationFailed
      })
  }

  override def persistenceId = "bot-labyrinth-database"
}

object LabyrinthActor {

  //events
  trait Event

  case class Finish(id: Long) extends Event

  //queries
  case class MakeLabyrinth(id: Long, width: Int, height: Int)

  case class MakeLabyrinthResult(result: GenerationResult)

  case class GetPosition(id: Long)

  case class Position(position: (Int, Int))

  case class Go(id: Long, direction: Direction, steps: Int)

  case class GoResult(result: MoveResult)
}
