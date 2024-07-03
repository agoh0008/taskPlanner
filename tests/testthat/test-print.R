library(testthat)
library(taskPlanner)

# Sample TodoList object for testing
sample_todo_list <- list(
  task = c("Task 1", "Task 2", "Task 3"),
  priority = c(1, 2, 3),
  due = as.Date(c("2024-05-10", "2024-05-15", "2024-05-20"))
)
class(sample_todo_list) <- "TodoList"

# Define tests
test_that("print.TodoList prints TodoList object correctly with numeric priority", {
  # Capture output of print function
  output <- capture.output(print(sample_todo_list))

  # Check if header is printed correctly
  expect_match(output[1], "Task\\s+Priority\\s+Due Date")

  # Check if task entries are printed correctly
  expect_match(output[3], "Task 1\\s+1\\s+2024-05-10")
  expect_match(output[4], "Task 2\\s+2\\s+2024-05-15")
  expect_match(output[5], "Task 3\\s+3\\s+2024-05-20")
})
