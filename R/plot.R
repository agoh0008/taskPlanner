#' @name plot
#' @title Plot Todo-List
#' @param x An object of class 'TodoList'.
#' @param ... additional arguments
#' @return Nothing.
#'
#' @examples
#'
#' # Plotting TodoList objects:
#'
#' list <- import_todo_list()
#' my_todo_list <- create_todo_list(list)
#' plot(my_todo_list)
#'
#' @import
#' viridisLite
#' graphics
#' @export
plot.TodoList <- function(x, ...) {
  # Define color palette for priority levels
  colors <- viridisLite::viridis(5)

  # Calculate the number of bars
  num_bars <- length(x$task)

  # Set the bottom margin based on the number of bars
  bottom_margin <- max(5, num_bars * 0.5)

  # Set the bar width based on the number of bars
  bar_width <- 0.8 / num_bars  # Adjust as needed

  # Set the margins
  par(mar = c(bottom_margin + 1, 6, 4, 2))

  # Plotting using barplot with adjusted margins
  b <- barplot(x$priority, names.arg = rep("", length(x$task)), col = colors[x$priority],
               main = "To-do List", xlab = "Task", ylab = "Priority",
               ylim = c(0, max(x$priority) * 1.2), las = 2, cex.names = 0.8, width = bar_width)

  # Add x-axis labels with rotation
  text(x = b, y = par("usr")[3] - 0.3, srt = 45, adj = 1, labels = x$task, xpd = TRUE, cex = 0.8)

  # Remove y-axis labels
  axis(2, labels = FALSE)
}

