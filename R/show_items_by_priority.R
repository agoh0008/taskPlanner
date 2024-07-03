#' @name show_items_by_priority
#' @title Show Items Based on Priority Filter Criteria
#' @description
#' This generic function displays items from a to-do list or other applicable classes based on a specified priority filter criteria.
#'
#' @param x An object of class 'TodoList' or other applicable classes.
#' @param priority The priority level of the tasks to filter by, an integer between 1 and 5.
#' @param ... additional arguments
#' @return Nothing. The function displays items from a to-do list or other applicable classes based on the specified priority filter criteria.
#' @examples
#'
#' # Showing items with the specified priority for TodoList objects:
#' list <- import_todo_list()
#' my_todo_list <- create_todo_list(list)
#' show_items_by_priority(my_todo_list, priority = 3)
#'
#' # Showing items with the specified priority for other classes:
#' other_object <- list(task = paste("Task", 1:15),
#' priority = sample(1:5, 15, replace = TRUE),
#' due = sample(seq(as.Date("2022-01-01"), as.Date("2025-12-31"), by = "day"), 15))
#'
#' show_items_by_priority(other_object, priority = 2)
#'
#'
#' @export
show_items_by_priority <- function(x, priority, ...) {
  UseMethod("show_items_by_priority")
}

#' Method for showing items with the specified priority for TodoList objects
#'
#' @param x An object of class 'TodoList'.
#' @param priority The priority level to filter by, an integer between 1 and 5.
#' @param ... additional arguments
#' @export
show_items_by_priority.TodoList <- function(x, priority, ...) {

  if (missing(priority)) {
    stop("Please provide a priority number between 1 to 5.")
  }

  if (!is.numeric(priority) || priority < 1 || priority > 5) {
    stop("Priority must be an integer between 1 and 5.")
  }

  filtered_indices <- x$priority == as.numeric(priority)

  filtered_items <- list(
    Task = x$task[filtered_indices],
    Priority = x$priority[filtered_indices],
    Due_Date = x$due[filtered_indices]
  )

  if (any(filtered_indices)) {
    cat("Filtered items:\n")
    df <- data.frame(filtered_items)
    row.names(df) <- NULL  # Reset row names
    colnames(df) <- c("Task", "Priority", "Due_Date")
    print(df)
  } else {
    cat("No items matching the filter criteria.\n")
  }
}

#' Default method for showing items with the specified priority for other classes
#'
#' @param x An object of other applicable classes.
#' @param priority The priority level to filter by, an integer between 1 and 5.
#' @param ... additional arguments
#' @export
show_items_by_priority.default <- function(x, priority, ...) {
  if ("task" %in% names(x) && "priority" %in% names(x) && "due" %in% names(x)) {

    if (missing(priority)) {
      stop("Please provide a priority number between 1 to 5.")
    }

    if (!is.numeric(priority) || priority < 1 || priority > 5) {
      stop("Priority must be an integer between 1 and 5.")
    }

    if ("priority" %in% names(x)) {
      filtered_indices <- x$priority == priority
    } else {
      stop("The provided object does not have a 'priority' field.")
    }

    filtered_indices <- x$priority == as.numeric(priority)

    filtered_items <- list(
      Task = x$task[filtered_indices],
      Priority = x$priority[filtered_indices],
      Due_Date = if ("due" %in% names(x)) x$due[filtered_indices] else NA
    )

    if (any(filtered_indices)) {
      cat("Filtered items:\n")
      df <- data.frame(filtered_items)
      row.names(df) <- NULL  # Reset row names
      colnames(df) <- c("Task", "Priority", "Due_Date")
      print(df)
    } else {
      cat("No items matching the filter criteria.\n")
    }
  } else {
    stop("show_items_by_priority() method not implemented for this class.\n")
  }
}
