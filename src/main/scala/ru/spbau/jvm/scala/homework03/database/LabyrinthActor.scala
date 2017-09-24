package ru.spbau.jvm.scala.homework03.database

import akka.persistence.PersistentActor

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class LabirinthActor extends PersistentActor {

  import LabirinthActor._

  val map: mutable.HashMap[Long, ArrayBuffer[(String, String)]] =
    mutable.HashMap.empty

  def receiveEvent(event: Event): Unit = {
    event match {
      case AddWord(id, word, translation) =>
        map.getOrElseUpdate(id, ArrayBuffer.empty) +=
          ((word, translation))
    }
  }

  override def receiveRecover: Receive = {
    case evt: Event => receiveEvent(evt)
  }

  override def receiveCommand: Receive = {
    case evt: Event => persist(evt)(receiveEvent)
    case GetWords(id) =>
      sender ! Words(map.getOrElse(id, ArrayBuffer.empty))
    case GetWordToLearn(id) =>
      map.get(id) match {
        case Some(buffer) =>
          val (word, translation) =
            buffer(Random.nextInt(buffer.length))
          sender ! WordToLearn(word, translation)
        case None =>
          sender ! WordToLearn("word", "слово")
      }
  }

  override def persistenceId = "au-lecture-database"
}

object LabirinthActor {

  //events
  trait Event

  case class AddWord(id: Long, word: String, translation: String) extends Event

  //queries
  case class GetWordToLearn(id: Long)

  case class GetWords(id: Long)

  case class WordToLearn(word: String, translation: String)

  case class Words(buffer: ArrayBuffer[(String, String)])

}
