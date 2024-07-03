#' @name print
#' @title Print Todo-List
#' @param x An object of class 'TodoList'
#' @param ... additional arguments
#' @return Nothing.
#'
#' @examples
#'
#' # Printing TodoList objects:
#'
#' list <- import_todo_list()
#' my_todo_list <- create_todo_list(list)
#' print(my_todo_list)
#'
#' @export
print.TodoList <- function(x, ...) {
  # Print header
  cat("Task\t\tPriority\tDue Date\n")
  cat("------------------------------------------------------------\n")

  # Print each task entry
  for (i in seq_along(x$task)) {
    cat(format(x$task[i], width = 20, justify = "left"), "\t")
    cat(format(x$priority[i], width = 8, justify = "left"), "\t")
    cat(format(x$due[i], "%Y-%m-%d"), "\n")
  }
}
