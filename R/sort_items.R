#' @name sort_items
#' @title Sort Items by Date or Priority
#' @description
#' This function sorts items based on specified criteria such as date or priority.
#'
#' @param x An object of class 'TodoList' or other applicable classes.
#' @param date Logical, indicating whether to sort by date. If TRUE, sorting by date; if FALSE, sorting by priority.
#' @param ascending Logical, indicating the sorting order. If TRUE, ascending order; if FALSE, descending order.
#' @param ... additional arguments
#' @return The sorted object.
#'
#' @examples
#'
#' # Sorting a TodoList by date in ascending order
#'
#' list <- import_todo_list()
#' my_todo_list <- create_todo_list(list)
#'
#' sort_items(my_todo_list, date = TRUE, ascending = TRUE)
#'
#' # Sorting a TodoList by priority in descending order
#' list <- import_todo_list()
#' my_todo_list <- create_todo_list(list)
#' sort_items(my_todo_list, date = FALSE, ascending = FALSE)
#'
#' @export
sort_items <- function(x, date = TRUE, ascending = TRUE, ...) {
  UseMethod("sort_items")
}

#' Method for sorting items in a TodoList object
#'
#' @param x An object of class 'TodoList'.
#' @param date Logical, indicating whether to sort by date.
#' @param ascending Logical, indicating the sorting order.
#' @param ... additional arguments
#' @return The sorted TodoList object.
#'
#' @export
sort_items.TodoList <- function(x, date = TRUE, ascending = TRUE, ...) {

  if (!is.logical(date) || !is.logical(ascending)) {
    stop("Arguments 'date' and 'ascending' must be logical values.")
  }

  if (date) {
    if (ascending) {
      sorted_indices <- order(x$due, x$priority)
    } else {
      sorted_indices <- order(x$due, -x$priority)
    }
  } else {
    if (ascending) {
      sorted_indices <- order(x$priority)
    } else {
      sorted_indices <- order(-x$priority)
    }
  }

  # Reorder the tasks, priorities, and due dates based on sorted indices
  sorted_tasks <- x$task[sorted_indices]
  sorted_priorities <- x$priority[sorted_indices]
  sorted_dates <- x$due[sorted_indices]

  # If sorting by date in descending order, reverse the order of dates
  if (!ascending && date) {
    sorted_tasks <- rev(sorted_tasks)
    sorted_priorities <- rev(sorted_priorities)
    sorted_dates <- rev(sorted_dates)
  }

  # Print the sorted object
  cat("Sorted To-Do List by", ifelse(date, "date", "priority"),
      "in", ifelse(ascending, "ascending", "descending"), "order:\n")

  # Convert to data frame
  sorted_df <- data.frame(
    Task = sorted_tasks,
    Priority = sorted_priorities,
    Due_Date = sorted_dates
  )

  # Print as a table
  print(sorted_df)
}



#' Default method for sorting items in other applicable classes
#'
#' @param x An object of other applicable classes.
#' @param date Logical, indicating whether to sort by date.
#' @param ascending Logical, indicating the sorting order.
#' @param ... additional arguments
#' @return The sorted object.
#'
#' @export
sort_items.default <- function(x, date = TRUE, ascending = TRUE, ...) {

  if ("task" %in% names(x) && "priority" %in% names(x) && "due" %in% names(x)) {

    if (!is.logical(date) || !is.logical(ascending)) {
      stop("Arguments 'date' and 'ascending' must be logical values.")
    }

    if (date) {
      if (ascending) {
        sorted_indices <- order(x$due, x$priority)
      } else {
        sorted_indices <- order(x$due, -x$priority)
      }
    } else {
      if (ascending) {
        sorted_indices <- order(x$priority)
      } else {
        sorted_indices <- order(-x$priority)
      }
    }

    # Reorder the tasks, priorities, and due dates based on sorted indices
    sorted_tasks <- x$task[sorted_indices]
    sorted_priorities <- x$priority[sorted_indices]
    sorted_dates <- x$due[sorted_indices]

    # If sorting by date in descending order, reverse the order of dates
    if (!ascending && date) {
      sorted_tasks <- rev(sorted_tasks)
      sorted_priorities <- rev(sorted_priorities)
      sorted_dates <- rev(sorted_dates)
    }

    # Print the sorted object
    cat("Sorted To-Do List by", ifelse(date, "date", "priority"),
        "in", ifelse(ascending, "ascending", "descending"), "order:\n")

    # Convert to data frame
    sorted_df <- data.frame(
      Task = sorted_tasks,
      Priority = sorted_priorities,
      Due_Date = sorted_dates
    )

    # Print as a table
    print(sorted_df)
  } else {
    stop("sort_items() method not implemented for this class.\n")
  }
}
