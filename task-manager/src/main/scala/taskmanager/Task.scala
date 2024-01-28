package taskmanager

case class Task(id: Option[Int], description: String, status: TaskStatus)