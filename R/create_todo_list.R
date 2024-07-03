#' @name create_todo_list
#' @title Create a To-Do List S3 Object
#' @description
#' This function creates an S3 object of class TodoList containing all entries
#' from the provided to-do list.
#'
#' @param todo_list A data frame containing the to-do list with columns: task, priority, and due.
#' If not provided, the function will import the current to-do list.
#' @param ... additional arguments
#' @return An instance of the TodoList S3 class.
#'
#' @examples
#' # Example 1: Creating a TodoList object from an existing to-do list data frame:
#' list <- import_todo_list()
#' my_todo_list <- create_todo_list(list)
#'
#' # Example 2: Creating a TodoList object from the current to-do list (imported from package data):
#' my_todo_list <- create_todo_list()
#'
#' @importFrom taskPlanner import_todo_list
#'
#' @export
create_todo_list <- function(todo_list = NULL, ...) {
  # Check if todo_list is provided and has the required columns
  if (is.null(todo_list)) {
    # If no to-do list is provided, import the current to-do list
    todo_list <- import_todo_list()
  } else {
    # Check if the input dataframe has the required columns
    required_cols <- c("task", "priority", "due")
    if (!all(required_cols %in% names(todo_list))) {
      stop("Input dataframe must have columns: task, priority, and due.")
    }

    # Check if columns are in the specified format
    if (!all(sapply(todo_list$task, is.character)) ||
        !all(sapply(todo_list$priority, is.numeric)) ||
        !inherits(todo_list$due, "Date")) {
      stop("Columns 'task' must be in 'character' format, 'priority' must be in 'numeric' format, and 'due' must be in 'Date' format.")
    }
  }

  # Convert due dates to Date class (if not already)
  if (!inherits(todo_list$due, "Date")) {
    todo_list$due <- as.Date(todo_list$due)
  }

  # Assign class "TodoList" to the todo_list data frame
  class(todo_list) <- "TodoList"

  return(todo_list)
}
