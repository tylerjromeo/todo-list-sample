package com.example

import com.example.jdbc.TaskDAO
import com.example.jdbc.TaskDAO.Task
import com.example.jdbc.TaskDAO.Task._
import org.specs2.mutable.Specification
import org.specs2.time.NoTimeConversions
import spray.testkit.Specs2RouteTest
import spray.http._
import StatusCodes._
import scala.concurrent._
import scala.concurrent.duration._
import spray.httpx.SprayJsonSupport._

class TaskServiceSpec extends Specification with Specs2RouteTest with TaskService with NoTimeConversions {
  def actorRefFactory = system

  //  def before = Await.result(TaskDAO.setup(testData), 0.nanos)
  def before = println("before")


  // since we are inserting into a fresh database, it's safe to assume these values will have ids of 1, 2 and 3 even though the values are determined by the database
  val testData = Seq(Task(Some(1), "Do the first thing", complete = true, user = Some("test")), Task(Some(2), "Do the second thing", complete = false, user = Some("test")), Task(Some(3), "Do the third thing", complete = false, user = Some("test")))
  val postData = Task(None, "Do another thing", complete = true, user = None)

  sequential //TODO: set these up with mockito rather than data checking

  "TaskService" should {

    "GET /tasks should contain our test data" in {
      //first setup the test data
      Await.result(TaskDAO.setup(testData), 5 seconds)
      Get("/test/tasks") ~> taskRoute ~> check {
        status === OK
        val response = responseAs[Seq[Task]]
        response must containTheSameElementsAs(testData)
      }
    }

    "POST /tasks should create a new item with a new ID" in {
      Get("/test/tasks") ~> taskRoute ~> check {
        status === OK
        val initialResponse = responseAs[Seq[Task]]
        Post("/test/tasks", postData) ~> taskRoute ~> check {
          status === Created
          Get("/test/tasks") ~> taskRoute ~> check {
            status === OK
            val response = responseAs[Seq[Task]]
            response must containTheSameElementsAs(initialResponse :+ postData.copy(user = Some("test"), id = getNextId(initialResponse)))
          }
        }
      }
    }

    "POST /tasks with one name in the prefex should only return that data for a GET with that prefix" in {
      Get("/test/tasks") ~> taskRoute ~> check {
        status === OK
        val initialResponse = responseAs[Seq[Task]]
        Post("/test/tasks", postData) ~> taskRoute ~> check {
          status === Created
          Get("/test2/tasks") ~> taskRoute ~> check {
            status === OK
            val response = responseAs[Seq[Task]]
            response must not contain postData.copy(user = Some("test2"))
            Get("/test/tasks") ~> taskRoute ~> check {
              status === OK
              val response = responseAs[Seq[Task]]
              response must containTheSameElementsAs(initialResponse :+ postData.copy(user = Some("test"), id = getNextId(initialResponse)))
            }
          }
        }
      }
    }

    "GET /tasks/{id} should return the item with that id" in {
      Get("/test/tasks/1") ~> taskRoute ~> check {
        status === OK
        val response = responseAs[Task]
        response === testData(0)
      }
    }

    "PUT /tasks/{id} should update the item at that id" in {
      Get("/test/tasks/1") ~> taskRoute ~> check {
        status === OK
        val response = responseAs[Task]
        //set the complete value to false and change the test
        val putData = response.copy(complete = !response.complete, text = response.text.toUpperCase)
        Put("/test/tasks/1", putData) ~> taskRoute ~> check {
          status === Created
          Get("/test/tasks/1") ~> taskRoute ~> check {
            status === OK
            val response = responseAs[Task]
            response === putData
          }
        }
      }
    }

    "DELETE /tasks/{id} should remove the item at that id" in {
      Get("/test/tasks") ~> taskRoute ~> check {
        status === OK
        val response = responseAs[Seq[Task]]
        val taskToDelete = response.head
        taskToDelete.id must beSome[Int]
        val id = taskToDelete.id match {
          case None => -1000 //should never happen. If it does the request will fail
          case Some(x) => x
        }
        Delete(s"/test/tasks/${id}") ~> taskRoute ~> check {
          status === NoContent
          Get("/test/tasks") ~> taskRoute ~> check {
            status === OK
            val response = responseAs[Seq[Task]]
            response must not contain (taskToDelete)
          }
        }
      }
    }
  }

  def getNextId(tasks:Seq[Task]): Option[Int] ={
    max(tasks map {
      case Task(Some(id), _, _, _) => id
    }).map(1+)
  }

  def max(xs: Seq[Int]): Option[Int] = {
    xs match {
      case Nil => None
      case Seq(x: Int) => Some(x)
      case x :: y :: rest => max( (if (x > y) x else y) :: rest )
    }
  }
}
