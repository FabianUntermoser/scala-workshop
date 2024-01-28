package taskmanager

case class TaskManager(tasks: List[Task] = List()) {

  def addTask(description: String, status: TaskStatus): TaskManager = {
    val id = tasks match {
      case Nil => 1
      case _ => tasks.map(_.id.getOrElse(-1)).max + 1
    }
    val newTask = Task(Some(id), description, status)
    TaskManager(tasks :+ newTask)
  }

  def deleteTask(id: Int): Either[String, TaskManager] = {
    val taskIndex = tasks.indexWhere(_.id.contains(id))
    if (taskIndex == -1) {
      Left(s"Task with id $id not found")
    } else {
      val (before, after) = tasks.splitAt(taskIndex)
      val updatedTasks = before ++ after.tail
      Right(TaskManager(updatedTasks))
    }
  }

  def completeTask(i: Int): Either[String, TaskManager] = {
    val taskIndex = tasks.indexWhere(_.id.contains(i))
    if (taskIndex == -1) {
      Left(s"Task not found: $i")
    } else {
      val (before, after) = tasks.splitAt(taskIndex)
      val updatedTask = after.head.copy(status = Completed)
      val updatedTasks = before ++ (updatedTask :: after.tail)
      Right(TaskManager(updatedTasks))
    }
  }

  def completeTaskByName(str: String): Either[String, TaskManager] = {
    val taskIndex = tasks.indexWhere(_.description == str)
    if (taskIndex == -1) {
      Left(s"Task not found: $str")
    } else {
      val (before, after) = tasks.splitAt(taskIndex)
      val updatedTask = after.head.copy(status = Completed)
      val updatedTasks = before ++ (updatedTask :: after.tail)
      Right(TaskManager(updatedTasks))
    }
  }

  def batchUpdate(updateFunction: Task => Task): TaskManager = {
    val updatedTasks = tasks.map(updateFunction)
    TaskManager(updatedTasks)
  }

  def listTasks(status: TaskStatus): List[Task] = {
    tasks.filter(_.status == status)
  }

}

object TaskManager {
}
