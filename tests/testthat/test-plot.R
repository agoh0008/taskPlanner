library(testthat)
library(taskPlanner)

# Define test case for plot.TodoList function
test_that("plot.TodoList produces plot without error", {
  # Create a sample TodoList object
  sample_todo_list <- list(
    task = c("Task 1", "Task 2", "Task 3"),
    priority = c(1, 2, 3),
    due = as.Date(c("2024-05-10", "2024-05-15", "2024-05-20"))
  )
  class(sample_todo_list) <- "TodoList"

  # Check if plot function runs without error
  expect_silent(plot(sample_todo_list))

})
