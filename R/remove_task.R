#' @name remove_task
#' @title Remove Task Items from To-Do List
#' @description
#' This function allows users to remove a specific item from their to-do list.
#'
#' @param task_index An integer specifying the index of the task to be removed.
#' @param todo_list A data frame or list representing the current to-do list.
#' @param ... additional arguments
#'
#' @return An updated to-do list data frame or list with the specified item removed.
#'
#' @examples
#'
#' list <- import_todo_list()
#' list <- add_task(list, "Buy bread", 3, "2024-03-15")
#'
#' list <- remove_task(1, list)
#'
#' @export

remove_task <- function(task_index, todo_list, ...) {

  validate_inputs <- function(task_index, todo_list) {
    if (missing(todo_list)) {
      stop("To-do list is missing. Use the import_todo_list function first or specify the list argument.")
    }

    if (is.data.frame(todo_list)) {
      if (!is.numeric(task_index) || task_index < 1) {
        stop("Invalid task index. Task index must be a numeric value greater than or equal to 1.")
      }

      if (task_index > nrow(todo_list)) {
        stop("Invalid task index. Task index exceeds the number of tasks in the current to-do list.")
      }
    } else if (is.list(todo_list)) {
      if (!is.numeric(task_index) || task_index < 1) {
        stop("Invalid task index. Task index must be a numeric value greater than or equal to 1.")
      }

      if (task_index > length(todo_list$task)) {
        stop("Invalid task index. Task index exceeds the number of tasks in the current to-do list.")
      }
    } else {
      stop("Invalid to-do list format. It must be either a list or a data frame.")
    }
  }

  # Validate inputs
  validate_inputs(task_index, todo_list)

  # Remove the specified item from the to-do list
  if (is.data.frame(todo_list)) {
    updated_todo_list <- todo_list[-task_index, , drop = FALSE]
    rownames(updated_todo_list) <- NULL
  } else {
    updated_todo_list <- list(
      task = todo_list$task[-task_index],
      priority = todo_list$priority[-task_index],
      due = todo_list$due[-task_index]
    )
  }

  # Preserve the class attribute of the original object
  class(updated_todo_list) <- class(todo_list)

  return(updated_todo_list)
}
