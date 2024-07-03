#' @name show_items_by_date
#' @title Show Items Based on Date Filter Criteria
#' @description
#' This generic function displays items from a to-do list or other applicable classes based on a specified date filter criteria.
#'
#' @param x An object of class 'TodoList' or other applicable classes.
#' @param date The date to filter by, in the format "YYYY-MM-DD".
#' @param ... additional arguments
#'
#' @return Nothing. The function displays items from a to-do list or other applicable classes based on the specified date filter criteria.
#'
#' @examples
#'
#' # Showing items due on the specified date for TodoList objects:
#'
#' list <- import_todo_list()
#' my_todo_list <- create_todo_list(list)
#' show_items_by_date(my_todo_list, date = "2024-03-15")
#'
#' # Showing items due on the specified date for other classes:
#'
#' other_object <- list(task = paste("Task", 1:15),
#' priority = sample(1:5, 15, replace = TRUE),
#' due = sample(seq(as.Date("2022-01-01"), as.Date("2025-12-31"), by = "day"), 15))
#'
#' show_items_by_date(other_object, date = "2022-01-01")
#'
#' @export
show_items_by_date <- function(x, date = NULL, ...) {
  UseMethod("show_items_by_date")
}

#' Method for showing items due on the specified date for TodoList objects
#'
#' @param x An object of class 'TodoList'.
#' @param date The date to filter by, in the format "YYYY-MM-DD".
#' @param ... additional arguments
#' @export
show_items_by_date.TodoList <- function(x, date = NULL, ...) {

  if (is.null(date)) {
    stop("Please provide a date in the format 'YYYY-MM-DD'.")
  }

  if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", date)) {
    stop("Please provide a valid date in the format 'YYYY-MM-DD'.")
  }

  filtered_indices <- x$due == as.Date(date)

  filtered_items <- list(
    Task = x$task[filtered_indices],
    Priority = x$priority[filtered_indices],
    Due_Date = x$due[filtered_indices]
  )

  if (any(filtered_indices)) {
    cat("Filtered items:\n")
    df <- data.frame(filtered_items)
    row.names(df) <- NULL
    colnames(df) <- c("Task", "Priority", "Due_Date")
    print(df)
  } else {
    cat("No items matching the filter criteria.\n")
  }
}

#' Default method for showing items due on the specified date for other classes
#'
#' @param x An object of other applicable classes.
#' @param date The date to filter by, in the format "YYYY-MM-DD".
#' @param ... additional arguments
#' @export
show_items_by_date.default <- function(x, date = NULL, ...) {
  if ("task" %in% names(x) && "priority" %in% names(x) && "due" %in% names(x)) {
    if (is.null(date)) {
      stop("Please provide a date in the format 'YYYY-MM-DD'.")
    }

    if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", date)) {
      stop("Please provide a valid date in the format 'YYYY-MM-DD'.")
    }

    filtered_indices <- x$due == as.Date(date)

    filtered_items <- list(
      Task = x$task[filtered_indices],
      Priority = x$priority[filtered_indices],
      Due_Date = x$due[filtered_indices]
    )

    if (any(filtered_indices)) {
      cat("Filtered items:\n")
      df <- data.frame(filtered_items)
      row.names(df) <- NULL
      colnames(df) <- c("Task", "Priority", "Due_Date")
      print(df)
    } else {
      cat("No items matching the filter criteria.\n")
    }
  } else {
    stop("show_items_by_date() method not implemented for this class.\n")
  }
}
