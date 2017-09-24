package ru.spbau.jvm.scala.homework03

import akka.actor.{ActorSystem, Props}
import com.typesafe.akka.extension.quartz.QuartzSchedulerExtension
import ru.spbau.jvm.scala.homework03.database.LabyrinthActor
import ru.spbau.jvm.scala.homework03.bot.{AskActor, LabyrinthBot}

object Main extends App {
  val token = "424927383:AAFl-0Y8qmdiU_L-TWocRcVJxD8xBaDVokM"

  val system = ActorSystem()
  val scheduler = QuartzSchedulerExtension(system)
  val database = system.actorOf(Props(classOf[LabyrinthActor]))

  private val bot = new LabyrinthBot(token, database)
  val actor = system.actorOf(Props(classOf[AskActor], bot))

  scheduler.createSchedule("every minute", None, "	0/1 * * 1/1 * ? *")
  scheduler.schedule("every minute", actor, "Ask")

  bot.run()
}
