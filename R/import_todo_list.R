#' @name import_todo_list
#' @title Import To-Do List
#' @description
#' This function imports the to-do list from the "inst" directory of the package.
#'
#' If the to-do list file does not exist, it creates the file and initializes it with headers.
#'
#' @return The to-do list data frame.
#'
#' @examples
#' list <- import_todo_list()
#'
#' @importFrom utils read.csv
#'
#' @export

import_todo_list <- function() {
  todo_list_file <- system.file("todo_list.csv", package = "taskPlanner")

  if (!file.exists(todo_list_file)) {
    # If the file doesn't exist, return an empty data frame
    todo_list <- data.frame(task = character(),
                            priority = character(),
                            due = as.Date(character()),
                            stringsAsFactors = FALSE)
  } else {
    # Otherwise read the CSV file
    todo_list <- read.csv(todo_list_file, stringsAsFactors = FALSE)
    todo_list$due <- as.Date(todo_list$due)
  }

  return(todo_list)
}

