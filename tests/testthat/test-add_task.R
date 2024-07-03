library(testthat)
library(taskPlanner)

test_that("add_task function adds item to to-do list correctly", {

  sample_todo_list <- list(
    task = c("Do assignments", "Buy cake"),
    priority = c(3L, 4L),
    due = as.Date(c("2024-03-15", "2024-03-16"), format = "%Y-%m-%d"))

  # Test case 1: adding a new task
  updated_todo_list <- add_task(sample_todo_list, "Buy bread", 3, "2024-03-17")
  expect_equal(length(updated_todo_list$task), length(sample_todo_list$task) + 1)

  # Test case 2: check if the new task is added correctly
  expect_equal(updated_todo_list$task[length(updated_todo_list$task)], "Buy bread")
  expect_equal(updated_todo_list$priority[length(updated_todo_list$priority)], 3)
  expect_equal(updated_todo_list$due[length(updated_todo_list$due)], as.Date("2024-03-17"))

  # Test case 3: adding a task with invalid priority
  expect_error(add_task(sample_todo_list, "Do laundry", -2, "2024-03-18"),
               "Priority must be an integer between 1 and 5.")

  # Test case 4: adding a task with invalid due date format
  expect_error(add_task(sample_todo_list, "Clean room", 2, "18-03-2024"),
               "Provide a valid due date in 'YYYY-MM-DD' format.")

  # Test case 5: adding a task with empty task description
  expect_error(add_task(sample_todo_list, "", 1, "2024-03-19"),
               "Provide a valid task description containing at least one alphabetic character.")
})
