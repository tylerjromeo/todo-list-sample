package com.example.jdbc

/**
 * User: tylerromeo
 * Date: 5/5/15
 * Time: 2:25 PM
 *
 */


import slick.driver.PostgresDriver.api._
import spray.json._

object TaskDAO {
  //TODO unit tests

  val db = Database.forConfig("h2mem1")

  case class Task(
                   id: Option[Int],
                   text: String,
                   complete: Boolean,
                   user: Option[String]
                   )

  object Task extends DefaultJsonProtocol {
    implicit val taskFormat = jsonFormat4(Task.apply)
  }

  class Tasks(tag: Tag) extends Table[Task](tag, "tasks") {
    def id = column[Option[Int]]("task_id", O.PrimaryKey, O.AutoInc)

    def text = column[String]("text")

    def complete = column[Boolean]("complete")

    def user = column[Option[String]]("user")

    def * = (id, text, complete, user) <>((Task.apply _).tupled, Task.unapply)
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

  private def getTasksQuery(user: Option[String]) = user match {
    case Some(x) => tasksQuery.filter(_.user === x).result
    case None => tasksQuery.result
  }

  private def getTaskByIdQuery(taskId: Int) = tasksQuery.filter(_.id === taskId).result.headOption

  private def deleteTaskQuery(taskId: Int) = tasksQuery.filter(_.id === taskId).delete

  private def updateTaskQuery(task: Task) = tasksQuery.filter(_.id === task.id).update(task)

  def addTask(task: Task) = db.run(addTaskQuery(task))

  def getTasks(user: Option[String]) = db.run(getTasksQuery(user))

  def getTaskById(taskId: Int) = db.run(getTaskByIdQuery(taskId))

  def deleteTask(taskId: Int) = db.run(deleteTaskQuery(taskId))

  def updateTask(task: Task) = db.run(updateTaskQuery(task))

  def setup(tasks: Seq[Task]) = db.run(setupStatements(tasks))

}
