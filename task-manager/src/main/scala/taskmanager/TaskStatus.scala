package taskmanager

sealed trait TaskStatus

case object Pending extends TaskStatus

case object Completed extends TaskStatus
