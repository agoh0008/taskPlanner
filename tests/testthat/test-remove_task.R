library(testthat)
library(taskPlanner)

test_that("remove_task function removes item from to-do list correctly", {

  # Define sample todo list as a list
  sample_todo_list <- list(
    task = c("Do assignments", "Buy cake"),
    priority = c(3L, 4L),
    due = as.Date(c("2024-03-15", "2024-03-16"), format = "%Y-%m-%d")
  )

  # Define expected todo list after removing the first task
  expected_todo_list <- list(
    task = "Buy cake",
    priority = 4L,
    due = as.Date("2024-03-16")
  )

  # Remove the first task using remove_task function
  updated_todo_list <- remove_task(1, sample_todo_list)

  # Test case 1: check if the updated to-do list matches the expected to-do list
  expect_identical(updated_todo_list, expected_todo_list,
                   "Removed task should match the expected to-do list")

  # Test case 2: check if the task index is missing
  expect_error(remove_task(NULL, sample_todo_list),
               "Invalid task index. Task index must be a numeric value greater than or equal to 1.",
               info = "Task index should be provided.")

  # Test case 3: check if the task index is numeric
  expect_error(remove_task("one", sample_todo_list),
               "Invalid task index. Task index must be a numeric value greater than or equal to 1.",
               info = "Task index should be numeric.")

  # Test case 4: check if task index does not exist in to-do list
  expect_error(remove_task(10, sample_todo_list),
               "Invalid task index. Task index exceeds the number of tasks in the current to-do list.",
               info = "Task index should exist in the to-do list.")

})
