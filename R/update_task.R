#' @name update_task
#' @title Update Task Item in To-Do List
#' @description
#' This function allows users to update the task description, priority, or due date of an existing item in their to-do list.
#'
#' @param task_index An integer specifying the index of the task to be updated.
#' @param todo_list A data frame or list representing the current to-do list.
#' @param new_task A character string specifying the updated task description.
#' @param new_priority An integer specifying the updated priority of the task. Must be within the range 1-5.
#' @param new_due A Date object specifying the updated due date of the task.
#' @param ... additional arguments
#'
#'
#' @return An updated to-do list data frame or list with the specified item updated.
#'
#' @examples
#'
#' list <- import_todo_list()
#' list <- add_task(list, "Buy bread", 3, "2024-03-15")
#'
#' list <- update_task(1, list, "Buy milk", 4, "2024-03-18")
#'
#' @export

update_task <- function(task_index, todo_list, new_task = NULL, new_priority = NULL, new_due = NULL, ...) {

  validate_inputs <- function(todo_list, task_index, new_task, new_priority, new_due) {
    if (missing(todo_list) || !is.list(todo_list)) {
      stop("To-do list is missing or has an invalid format.")
    }

    if (missing(task_index) || !is.numeric(task_index) || task_index < 1) {
      stop("Invalid task index. Task index must be a numeric value greater than or equal to 1.")
    }

    if (task_index > length(todo_list$task)) {
      stop("Invalid task index. Task index exceeds the number of tasks in the current to-do list.")
    }

    if (!is.null(new_task) && (!is.character(new_task) || nchar(new_task) == 0 || !grepl("[A-Za-z]", new_task))) {
      stop("Provide a valid task description containing at least one alphabetic character.")
    }

    if (!is.null(new_priority) && (!is.numeric(new_priority) || new_priority < 1 || new_priority > 5)) {
      stop("Priority must be a numeric value within the range 1-5.")
    }

    if (!is.null(new_due)) {
      if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", new_due)) {
        stop("Provide a valid due date in 'YYYY-MM-DD' format.")
      }
      new_due <- as.Date(new_due, format = "%Y-%m-%d")
      if (is.na(new_due)) {
        stop("Provide a valid due date in 'YYYY-MM-DD' format.")
      }
    }
  }

  validate_inputs(todo_list, task_index, new_task, new_priority, new_due)

  # Check if a record with the new values already exists
  existing_task <- todo_list$task[[task_index]]
  existing_priority <- todo_list$priority[[task_index]]
  existing_due <- todo_list$due[[task_index]]

  for (i in seq_along(todo_list$task)) {
    if (i != task_index &&
        todo_list$task[[i]] == new_task &&
        todo_list$priority[[i]] == new_priority &&
        todo_list$due[[i]] == new_due) {
      stop("A record with the new values already exists.")
    }
  }

  # Update the task attributes if provided
  if (!is.null(new_task)) {
    todo_list$task[[task_index]] <- new_task
  }
  if (!is.null(new_priority)) {
    todo_list$priority[[task_index]] <- new_priority
  }
  if (!is.null(new_due)) {
    todo_list$due[[task_index]] <- as.Date(new_due, format = "%Y-%m-%d")
  }

  # Return the updated to-do list
  return(todo_list)
}
