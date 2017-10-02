package ru.spbau.jvm.scala
package lecture03

import akka.actor.{ActorSystem, Props}
import com.typesafe.akka.extension.quartz.QuartzSchedulerExtension
import ru.spbau.jvm.scala.lecture03.bot.{AskActor, AuLectureBot}
import ru.spbau.jvm.scala.lecture03.database.AuLectureActor

object Main extends App {
  val token = "310396870:AAGsGH1mKThgn19pI3Tje4xSGa3G3p7SvBE"

  val system = ActorSystem()
  val scheduler = QuartzSchedulerExtension(system)
  val database = system.actorOf(Props(classOf[AuLectureActor]))

  private val bot = new AuLectureBot(token, database)
  val actor = system.actorOf(Props(classOf[AskActor], bot))

  scheduler.createSchedule("every minute", None, "	0/1 * * 1/1 * ? *")
  scheduler.schedule("every minute", actor, "Ask")

  bot.run()
}
