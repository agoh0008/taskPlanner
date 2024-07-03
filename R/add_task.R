#' @name add_task
#' @title Add Task Items to Current To-Do List
#' @description
#' This function allows users to add a new item to their to-do list, specifying the task description, priority, and due date.
#'
#' @param todo_list A data frame or a list representing the current to-do list.
#' @param task A character string specifying the task to be added.
#' @param priority An integer specifying the priority of the task. Must be within the range 1-5.
#' @param due A Date object specifying the due date of the task.
#' @param ... additional arguments
#'
#' @return An updated to-do list data frame or list with the new item added.
#'
#' @examples
#' list <- import_todo_list()
#'
#' list <- add_task(list, "Buy bread", 3, "2024-03-15")
#' list <- add_task(list, "Do laundry", 2, "2024-03-16")
#' list <- add_task(list, "Buy dinner", 1, "2024-04-09")
#'
#' @import dplyr
#'
#' @export

add_task <- function(todo_list, task, priority, due, ...) {

  validate_inputs <- function(todo_list, task, priority, due) {
    if (!("task" %in% names(todo_list) && "priority" %in% names(todo_list) && "due" %in% names(todo_list))) {
      stop("To-do list must contain 'task', 'priority', and 'due' columns.")
    }

    if (missing(todo_list)) {
      stop("To-do list is missing. Use the import_todo_list function or specify the list argument.")
    }

    if (missing(task) || !is.character(task) || nchar(task) == 0 || !grepl("[A-Za-z]", task)) {
      stop("Provide a valid task description containing at least one alphabetic character.")
    }

    if (missing(priority) || !is.numeric(priority) || priority %% 1 != 0 || priority < 1 || priority > 5) {
      stop("Priority must be an integer between 1 and 5.")
    }

    if (missing(due) || !inherits(due, "Date")) {
      if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", due)) {
        stop("Provide a valid due date in 'YYYY-MM-DD' format.")
      }
      due <- as.Date(due, format = "%Y-%m-%d")
      if (is.na(due)) {
        stop("Provide a valid due date in 'YYYY-MM-DD' format.")
      }
    }
  }

  validate_inputs(todo_list, task, priority, due)

  # Check if the input is a data frame or a list
  if (is.data.frame(todo_list)) {
    # Check if the task already exists in the to-do list
    if (nrow(todo_list) > 0) {
      task_exists <- FALSE
      for (i in 1:nrow(todo_list)) {
        if (!is.na(todo_list[i, "task"]) && todo_list[i, "task"] == task &&
            !is.na(todo_list[i, "priority"]) && todo_list[i, "priority"] == priority &&
            !is.na(todo_list[i, "due"]) && todo_list[i, "due"] == due) {
          task_exists <- TRUE
          break
        }
      }

      if (task_exists) {
        warning("A similar task already exists in the to-do list.")
        return(todo_list)
      } else {
        # Add the new item to the existing to-do list
        updated_todo_list <- dplyr::bind_rows(todo_list, data.frame(task = as.character(task), priority = priority,
                                                                    due = as.Date(due, format = "%Y-%m-%d")))

        return(updated_todo_list)
      }
    } else {
      # If the list is empty, simply add the new task
      updated_todo_list <- data.frame(task = as.character(task), priority = priority,
                                      due = as.Date(due, format = "%Y-%m-%d"))
      return(updated_todo_list)
    }
  } else if (is.list(todo_list)) {
    # Check if the task already exists in the to-do list
    task_exists <- FALSE
    if ("task" %in% names(todo_list)) {
      for (i in seq_along(todo_list$task)) {
        if (!is.null(todo_list$task[[i]]) && todo_list$task[[i]] == task &&
            !is.null(todo_list$priority[[i]]) && todo_list$priority[[i]] == priority &&
            !is.null(todo_list$due[[i]]) && todo_list$due[[i]] == due) {
          task_exists <- TRUE
          break
        }
      }
    } else {
      task_exists <- task %in% todo_list
    }

    if (task_exists) {
      warning("A similar task already exists in the to-do list.")
      return(todo_list)
    } else {
      # Add the new item to the existing to-do list
      updated_todo_list <- list(
        task = c(todo_list$task, as.character(task)),
        priority = c(todo_list$priority, priority),
        due = c(todo_list$due, as.Date(due, format = "%Y-%m-%d"))
      )

      # Preserve the class attribute of the original object
      class(updated_todo_list) <- class(todo_list)

      return(updated_todo_list)
    }
  } else {
    stop("Invalid format for the to-do list. It should be either a data frame or a list.")
  }
}

