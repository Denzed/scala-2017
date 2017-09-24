package ru.spbau.jvm.scala
package lecture03
package bot

import akka.actor.{Actor, ActorRef}
import akka.pattern.ask
import akka.util.Timeout
import info.mukel.telegrambot4s.api.declarative.Commands
import info.mukel.telegrambot4s.api.{Polling, TelegramBot}
import ru.spbau.jvm.scala.lecture03.database.AuLectureActor._
import ru.spbau.jvm.scala.lecture03.parser.MessageParser
import ru.spbau.jvm.scala.lecture03.parser.messages.{AddWord => AddWordMessage, _}

import scala.collection.mutable
import scala.concurrent.duration.DurationInt
import scala.util.Success

class AskActor(bot: AuLectureBot) extends Actor {
  override def receive = {
    case _ => bot.askUsers()
  }
}

class AuLectureBot(val token: String,
                   val database: ActorRef) extends TelegramBot with Polling with Commands {
  def askUsers(): Unit = {

  }

  val map: mutable.HashMap[Long, String] = mutable.HashMap.empty

  onMessage {
    implicit message =>
      message.text.foreach { text =>
        if (map.contains(message.chat.id)) {
          if (text == map(message.chat.id)) {
            reply("Правильно! Молодец!")
          } else {
            reply("Неправильно! Не молодец!")
          }
          map.remove(message.chat.id)
        } else
          MessageParser.parse(text) match {
            case AddWordMessage(word, translation) =>
              database !
                AddWord(message.chat.id, word, translation)
              reply("Слово добавлено!:)")
            case MyWords =>
              implicit val timeout: Timeout = Timeout(1.second)
              (database ? GetWords(message.chat.id)).onComplete {
                case Success(Words(buffer)) =>
                  reply(buffer.map {
                    case (word, translation) => s"$word -> $translation"
                  }.mkString("\n"))
                case _ =>
                  reply("Ошибка базы данных!:(")
              }
            case CheckMe =>
              implicit val timeout: Timeout = Timeout(1.second)
              (database ? GetWordToLearn(message.chat.id)).onComplete {
                case Success(WordToLearn(word, translation)) =>
                  map += ((message.chat.id, translation))
                  reply(word + "?")
                case _ =>
                  reply("Ошибка базы данных!:(")
              }
            case WrongMessage =>
              reply("Неверная команда:(")
          }
      }
  }
}
