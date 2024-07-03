#' @name export_todo_list
#' @title Export To-Do List to CSV File
#' @description
#' This function exports the updated to-do list back to a CSV file in the "inst" directory.
#'
#' @param todo_list The to-do list data frame or list to be exported.
#' @param ... additional arguments
#'
#' @return Nothing. The to-do list is exported to a CSV file.
#'
#' @examples
#'
#' list <- import_todo_list()
#'
#' export_todo_list(list)
#'
#' @importFrom utils write.csv
#'
#' @export

export_todo_list <- function(todo_list, ...) {

  # Check if todo_list is provided
  if (missing(todo_list)) {
    stop("Missing to-do list argument. Please specify the current to-do list.")
  }

  # Check if todo_list is a data frame
  if (is.data.frame(todo_list)) {
    # Construct the path to the todo_list.csv file within the "inst" directory
    todo_list_file <- system.file("todo_list.csv", package = "taskPlanner")

    # Write the to-do list data frame to the CSV file
    write.csv(todo_list, file = todo_list_file, row.names = FALSE)
  } else if (inherits(todo_list, "TodoList")) {
    # Convert TodoList to data frame
    todo_list_df <- data.frame(task = todo_list$task,
                               priority = todo_list$priority,
                               due = todo_list$due)

    # Construct the path to the todo_list.csv file within the "inst" directory
    todo_list_file <- system.file("todo_list.csv", package = "taskPlanner")

    # Write the to-do list data frame to the CSV file
    write.csv(todo_list_df, file = todo_list_file, row.names = FALSE)
  } else if (is.list(todo_list)) {
    # Convert list to data frame
    todo_list_df <- as.data.frame(todo_list)

    # Construct the path to the todo_list.csv file within the "inst" directory
    todo_list_file <- system.file("todo_list.csv", package = "taskPlanner")

    # Write the to-do list data frame to the CSV file
    write.csv(todo_list_df, file = todo_list_file, row.names = FALSE)
  } else {
    stop("To-do list is missing or has an invalid format. Use the import_todo_list function first.")
  }
}

