package ru.spbau.jvm.scala.homework03.bot

import akka.actor.{Actor, ActorRef}
import akka.pattern.ask
import akka.util.Timeout
import info.mukel.telegrambot4s.api.declarative.Commands
import info.mukel.telegrambot4s.api.{Polling, TelegramBot}
import ru.spbau.jvm.scala.homework03.database.LabyrinthActor._
import ru.spbau.jvm.scala.homework03.database._
import ru.spbau.jvm.scala.homework03.database.labyrinth._
import ru.spbau.jvm.scala.homework03.parser.MessageParser
import ru.spbau.jvm.scala.homework03.parser.messages.{FinishMessage, GetPositionMessage, GoMessage, StartMessage}

import scala.concurrent.duration.DurationInt
import scala.util.Success

class AskActor(bot: LabyrinthBot) extends Actor {
  override def receive: PartialFunction[Any, Unit] = {
    case _ => bot.askUsers()
  }
}

class LabyrinthBot(val token: String,
                   val database: ActorRef) extends TelegramBot with Polling with Commands {
  def askUsers(): Unit = {
  }

  onMessage {
    implicit message =>
      message.text.foreach { text =>
        MessageParser.parse(text) match {
          case StartMessage(width, height) =>
            implicit val timeout: Timeout = Timeout(1.second)
            (database ? MakeLabyrinth(message.chat.id, width, height)).onComplete {
              case Success(MakeLabyrinthResult(result)) =>
                reply(result match {
                  case GenerationSuccessful =>
                    s"Generated labyrinth of size ${width}x$height"
                  case _                    =>
                    s"An error occurred at labyrinth generation. " ++
                      "Please, make sure it is at least " ++
                      s"${Labyrinth.minimumWidth}x${Labyrinth.minimumHeight}"
                })
              case _ => reply("Database error")
            }
          case GoMessage(direction, steps) =>
            implicit val timeout: Timeout = Timeout(1.second)
            (database ? Go(message.chat.id, direction, steps)).onComplete {
              case Success(GoResult(result)) =>
                reply(result match {
                  case MoveSuccessful => "OK"
                  case MoveBlockedByWall => "A wall is blocking the way. No movement commenced"
                  case MoveToExit(turns) =>
                    database !
                      Finish(message.chat.id)
                    s"You found a way out just in $turns turns!"
                  case NoLabyrinth => "You are not in a labyrinth right now, but feel free to generate one typing \"start\""
                })
              case _ =>
                reply("Database error")
            }
          case GetPositionMessage =>
            implicit val timeout: Timeout = Timeout(1.second)
            (database ? GetPosition(message.chat.id)).onComplete {
              case Success(Position((-1, -1))) =>
                reply("You are not in a labyrinth right now")
              case Success(Position(position)) =>
                reply(s"Your current position is $position")
              case _ =>
                reply("Database error")
            }
          case FinishMessage =>
            database !
              Finish(message.chat.id)
            reply("Destroyed your labyrinth")
        }
      }
  }
}
