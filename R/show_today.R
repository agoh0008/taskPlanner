#' @name show_today
#' @title Show Items due Today or Earlier
#' @description
#' This generic function shows items due today or earlier from a to-do list.
#' It also generates a bar plot showing the priority of tasks, with higher priority tasks indicated by taller bars.
#'
#' @param x An object of class 'TodoList' or other applicable classes representing a to-do list.
#' @param ... additional arguments
#' @return Nothing. The function displays items due today or earlier from a to-do list,
#' accompanied by a bar plot illustrating task priorities.
#'
#' @examples
#'
#' # Showing items due today or earlier for TodoList objects:
#'
#' list <- import_todo_list()
#' my_todo_list <- create_todo_list(list)
#' show_today(my_todo_list)
#'
#' # Showing items due today or earlier for objects of other classes:
#'
#' other_object <- list(task = paste("Task", 1:15),
#' priority = sample(1:5, 15, replace = TRUE),
#' due = sample(seq(as.Date("2022-01-01"), as.Date("2025-12-31"), by = "day"), 15))
#'
#' show_today(other_object)
#'
#' @importFrom graphics axis barplot
#' @import viridis
#'
#' @export
show_today <- function(x, ...) {
  UseMethod("show_today")
}

#' Method for showing items due today or earlier for TodoList objects
#'
#' @param x An object of class 'TodoList'.
#' @param ... additional arguments
#' @export
show_today.TodoList <- function(x, ...) {

  today <- Sys.Date()
  overdue <- x$due <= today

  if (any(overdue)) {
    cat("Items due today or earlier:\n")
    df <- data.frame(
      Task = x$task[overdue],
      Priority = x$priority[overdue],
      Due_Date = format(x$due[overdue], "%Y-%m-%d")
    )

    # Reorder the data frame by date (ascending)
    df <- df[order(df$Due_Date), ]

    print(df)

    # Define the number of colors needed based on the number of tasks
    num_colors <- nrow(df)
    # Generate a color palette using the viridis palette
    bar_colors <- viridis(num_colors)

    # Map priority to level of importance
    df$Level_of_Importance <- max(df$Priority) - df$Priority + 1

    # Plot the bar chart with customized colors and other visual attributes
    barplot(df$Level_of_Importance,
            names.arg = df$Task,
            col = bar_colors,
            border = "black",
            main = "Priority of Tasks",
            xlab = "Task",
            ylab = "Level of Priority",
            ylim = c(0, max(df$Level_of_Importance) * 1.2),
            cex.names = ifelse(nrow(df) > 5, 0.8, 1),
            las = ifelse(nrow(df) > 5, 2, 1),
            yaxt = "n"
    )

    # Add custom y-axis labels (reversed order)
    axis(2, at = c(1, max(df$Level_of_Importance)), labels = c("Least important", "Most important"))

  } else {
    cat("No items due today or earlier.\n")
  }
}


#' Default method for showing items due today or earlier for other classes
#'
#' @param x An object of other applicable classes.
#' @param ... additional arguments
#' @export
show_today.default <- function(x, ...) {
  if ("task" %in% names(x) && "priority" %in% names(x) && "due" %in% names(x)) {

    today <- Sys.Date()
    due_dates <- as.Date(x$due, format = "%Y-%m-%d")
    overdue <- due_dates <= today

    if (any(overdue)) {
      cat("Items due today or earlier:\n")
      df <- data.frame(
        Task = x$task[overdue],
        Priority = x$priority[overdue],
        Due_Date = format(due_dates[overdue], "%Y-%m-%d")
      )

      # Reorder the data frame by date (ascending)
      df <- df[order(df$Due_Date), ]

      print(df)

      # Define the number of colors needed based on the number of tasks
      num_colors <- nrow(df)
      # Generate a color palette using the viridis palette
      bar_colors <- viridis(num_colors)

      # Map priority to level of importance
      df$Level_of_Importance <- max(df$Priority) - df$Priority + 1

      # Plot the bar chart with customized colors and other visual attributes
      barplot(df$Level_of_Importance,
              names.arg = df$Task,
              col = bar_colors,
              border = "black",
              main = "Priority of Tasks",
              xlab = "Task",
              ylab = "Level of Priority",
              ylim = c(0, max(df$Level_of_Importance) * 1.2),
              cex.names = ifelse(nrow(df) > 5, 0.8, 1),
              las = ifelse(nrow(df) > 5, 2, 1),
              yaxt = "n"
      )

      # Add custom y-axis labels (reversed order)
      axis(2, at = c(1, max(df$Level_of_Importance)), labels = c("Least important", "Most important"))

    } else {
      cat("No items due today or earlier.\n")
    }
  } else {
    stop("show_today() method not implemented for this class.\n")
  }
}

