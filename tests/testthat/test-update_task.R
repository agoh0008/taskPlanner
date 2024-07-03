library(testthat)
library(taskPlanner)

test_that("update_task function updates task item in to-do list correctly", {

  sample_todo_list <- list(
    task = c("Do assignments", "Buy cake"),
    priority = c(3L, 4L),
    due = as.Date(c("2024-03-15", "2024-03-16"), format = "%Y-%m-%d")
  )

  # Update the first task in the sample to-do list
  expected_todo_list <- list(
    task = c("Finish assignments", "Buy cake"),
    priority = c(3L, 4L),
    due = as.Date(c("2024-03-15", "2024-03-16"), format = "%Y-%m-%d")
  )

  # Update task description using update_task function
  updated_todo_list <- update_task(1, sample_todo_list, new_task = "Finish assignments")

  # Test case 1: check if the updated to-do list matches the expected to-do list
  expect_identical(updated_todo_list, expected_todo_list,
                   "Updated task item should match the expected to-do list")

  # Test case 2: check if the task index is out of range
  expect_error(update_task(3, sample_todo_list, new_task = "Read book"),
               "Invalid task index. Task index exceeds the number of tasks in the current to-do list.",
               info = "Task index should be within the range of the to-do list.")

  # Test case 3: check if the task index is numeric
  expect_error(update_task("one", sample_todo_list, new_task = "Read book"),
               "Invalid task index.",
               info = "Task index must be a numeric value greater than or equal to 1.")

  # Test case 4: check if the todo_list is missing
  expect_error(update_task(1, NULL, new_task = "Read book"),
               "To-do list is missing.",
               info = "Use the import_todo_list function first or specify the list argument.")

  # Test case 5: check for invalid new_task format
  expect_error(update_task(1, sample_todo_list, new_task = 123),
               "Provide a valid task description containing at least one alphabetic character.",
               info = "Task description must contain at least one alphabetic character.")

  # Test case 6: check for invalid new_priority value
  expect_error(update_task(1, sample_todo_list, new_priority = 0),
               "Priority must be a numeric value within the range 1-5.",
               info = "Priority must be within the range 1-5.")

  # Test case 7: check for invalid new_due format
  expect_error(update_task(1, sample_todo_list, new_due = "2024-15-03"),
               "Provide a valid due date in 'YYYY-MM-DD' format.",
               info = "Due date must be in 'YYYY-MM-DD' format.")
})
