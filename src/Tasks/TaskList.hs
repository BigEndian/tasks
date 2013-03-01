module Tasks.TaskList(TaskList,taskListIsEmpty) where
import Tasks.Task


data TaskList = TaskList [Task]

taskListIsEmpty :: TaskList -> Bool
taskListIsEmpty (TaskList tsks) = length tsks == 0
