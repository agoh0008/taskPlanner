% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_task.R
\name{add_task}
\alias{add_task}
\title{Add Task Items to Current To-Do List}
\usage{
add_task(todo_list, task, priority, due, ...)
}
\arguments{
\item{todo_list}{A data frame or a list representing the current to-do list.}

\item{task}{A character string specifying the task to be added.}

\item{priority}{An integer specifying the priority of the task. Must be within the range 1-5.}

\item{due}{A Date object specifying the due date of the task.}

\item{...}{additional arguments}
}
\value{
An updated to-do list data frame or list with the new item added.
}
\description{
This function allows users to add a new item to their to-do list, specifying the task description, priority, and due date.
}
\examples{
list <- import_todo_list()

list <- add_task(list, "Buy bread", 3, "2024-03-15")
list <- add_task(list, "Do laundry", 2, "2024-03-16")
list <- add_task(list, "Buy dinner", 1, "2024-04-09")

}
