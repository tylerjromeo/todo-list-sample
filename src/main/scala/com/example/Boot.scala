package com.example

import akka.actor.{ActorSystem, Props}
import akka.io.IO
import com.example.jdbc.TaskDAO
import com.example.jdbc.TaskDAO.Task
import spray.can.Http
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._

object Boot extends App {

  // we need an ActorSystem to host our application in
  implicit val system = ActorSystem("on-spray-can")

  // create and start our service actor
  val service = system.actorOf(Props[TaskServiceActor], "task-service")

  implicit val timeout = Timeout(5.seconds)
  // start a new HTTP server on port 8080 with our service actor as the handler
  TaskDAO.setup(Seq())
  IO(Http) ? Http.Bind(service, interface = "localhost", port = 8080)
}
