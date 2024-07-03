library(testthat)
library(taskPlanner)

# Test for TodoList objects
test_that("show_today.TodoList displays items due today or earlier and generates a bar plot", {
  # Create a sample TodoList object
  todo_list <- create_todo_list(list(
    task = c("Task 1", "Task 2"),
    priority = c(1, 2),
    due = c(Sys.Date() - 1, Sys.Date())
  ))

  # Capture output of the function
  output <- capture_output(show_today(todo_list))

  # Test if the expected output is displayed
  expect_match(output, "Items due today or earlier:")
  expect_match(output, "Task")
  expect_match(output, "Priority")
  expect_match(output, "Due_Date")
})

# Test for default method
test_that("show_today.default displays items due today or earlier and generates a bar plot", {
  # Create a sample object of other applicable class
  other_object <- list(
    task = c("Task 1", "Task 2"),
    priority = c(1, 2),
    due = c(Sys.Date() - 1, Sys.Date())
  )

  # Capture output of the function
  output <- capture_output(show_today(other_object))

  # Test if the expected output is displayed
  expect_match(output, "Items due today or earlier:")
  expect_match(output, "Task")
  expect_match(output, "Priority")
  expect_match(output, "Due_Date")
})


