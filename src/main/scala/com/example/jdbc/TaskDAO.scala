package com.example.jdbc

/**
 * User: tylerromeo
 * Date: 5/5/15
 * Time: 2:25 PM
 *
 */


import slick.driver.PostgresDriver.api._
import spray.json._
import scala.concurrent.ExecutionContext.Implicits.global

object TaskDAO {
  //TODO unit tests

  val db = Database.forConfig("postgres")

  case class Task(
                   id: Option[Int],
                   text: String,
                   complete: Boolean
                   )

  object Task extends DefaultJsonProtocol {
    implicit val taskFormat = jsonFormat3(Task.apply)
  }

  class Tasks(tag: Tag) extends Table[Task](tag, "tasks") {
    def id = column[Option[Int]]("task_id", O.PrimaryKey, O.AutoInc)

    def name = column[String]("text")

    def complete = column[Boolean]("complete")

    def * = (id, name, complete) <>((Task.apply _).tupled, Task.unapply)
  }

  val tasksQuery = TableQuery[Tasks]

  val dropTables = tasksQuery.schema.drop

  val createTables = tasksQuery.schema.create


  private def testDataStatements(tasks: Seq[Task]) = {
    DBIO.sequence(tasks.map(t => addTaskQuery(t)))
  }

  private def setupStatements(tasks: Seq[Task]) =
    dropTables.asTry andThen DBIO.seq(
      createTables,
      testDataStatements(tasks)
    )


  private def addTaskQuery(task: Task) = (tasksQuery returning tasksQuery.map(_.id)) += task

  private def getTasksQuery = tasksQuery.result

  private def getTaskByIdQuery(taskId: Int) = tasksQuery.filter(_.id === taskId).result.headOption

  private def deleteTaskQuery(taskId: Int) = tasksQuery.filter(_.id === taskId).delete

  private def updateTaskQuery(task: Task) = tasksQuery.filter(_.id === task.id).update(task)

  private def insertOrUpdateTaskQuery(task: Task) = task.id match {
    case None => throw new Exception//fail
    case Some(id) => for {
      tOption <- getTaskByIdQuery(id)
      result <- tOption match {
        case None => tasksQuery forceInsert task //TODO: this needs to update the autoInc value
        case Some(_) => updateTaskQuery(task)
      }
    } yield result
  }

def addTask (task: Task) = db.run (addTaskQuery (task) )

def getTasks = db.run (getTasksQuery)

def getTaskById (taskId: Int) = db.run (getTaskByIdQuery (taskId) )

def deleteTask (taskId: Int) = db.run (deleteTaskQuery (taskId) )

def updateTask (task: Task) = db.run (insertOrUpdateTaskQuery (task) )

def setup (tasks: Seq[Task] ) = db.run (setupStatements (tasks) )

//  def main(args: Array[String]) {
//    try {
//      //      db.run(setup) onComplete {
//      //        case Success(x) => {
//      //          println("Success!!!")
//      //        }
//      //        case Failure(t) => {
//      //          println("Failure")
//      //          t.printStackTrace
//      //        }
//      //      }
//      println(Task(Some(1), "test", true).toJson.prettyPrint)
//      db.run(getTasks) onComplete {
//        case Success(x) => {
//          println("Success!!!")
//          x.map(task => {
//            println(s"${task.id} ${task.text} ${task.complete}")
//            println(task.toJson.prettyPrint)
//          })
//        }
//        case Failure(t) => {
//          println("Failure")
//          t.printStackTrace
//        }
//      }
//      //      println(Task(Some(1), "test", true).toJson.prettyPrint)
//      //            db.run(getTaskById(3)) onComplete {
//      //              case Success(x) => {
//      //                println("Success!!!")
//      //                println(x.toJson.prettyPrint)
//      //                x match {
//      //                  case None => println("no task for that id")
//      //                  case Some(task) => println(Task(Some(1), "test", true).toJson.prettyPrint) /*println(s"${task.id} ${task.text} ${task.complete}")*/
//      //                }
//      //              }
//      //              case Failure(t) => {
//      //                println("Failure")
//      //                t.printStackTrace
//      //              }
//      //            }
//      //      db.run(deleteTask(1)) onComplete {
//      //        case Success(x) => {
//      //          println("Success!!!")
//      //        }
//      //        case Failure(t) => {
//      //          println("Failure")
//      //          t.printStackTrace
//      //        }
//      //      }
//      //      db.run(updateTask(Task(Some(2), "blorgle", true))) onComplete {
//      //        case Success(x) => {
//      //          println("Success!!!")
//      //        }
//      //        case Failure(t) => {
//      //          println("Failure")
//      //          t.printStackTrace
//      //        }
//      //      }
//    } finally db.close()
//
//  }

}
