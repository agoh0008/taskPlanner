library(testthat)
library(taskPlanner)

# Load and create todo list
todo_list_data <- import_todo_list()
my_todo_list <- create_todo_list(todo_list_data)

# Tests for sort_items function
test_that("sort_items sorts TodoList by date or priority", {
  # Test sorting by date in ascending order
  sorted_todo_date_asc <- sort_items(my_todo_list, date = TRUE, ascending = TRUE)
  expect_true(all(sorted_todo_date_asc$due == sort(my_todo_list$due)))

  # Test sorting by date in descending order
  sorted_todo_date_desc <- sort_items(my_todo_list, date = TRUE, ascending = FALSE)
  expect_true(all(sorted_todo_date_desc$due == sort(my_todo_list$due, decreasing = TRUE)))

  # Test sorting by priority in ascending order
  sorted_todo_priority_asc <- sort_items(my_todo_list, date = FALSE, ascending = TRUE)
  expect_true(all(sorted_todo_priority_asc$priority == sort(my_todo_list$priority)))

  # Test sorting by priority in descending order
  sorted_todo_priority_desc <- sort_items(my_todo_list, date = FALSE, ascending = FALSE)
  expect_true(all(sorted_todo_priority_desc$priority == sort(my_todo_list$priority, decreasing = TRUE)))
})


# Tests for sort_items.default method
test_that("sort_items.default sorts objects by date or priority", {
  # Create a sample object for testing default method
  other_object <- list(
    task = paste("Task", 1:5),
    priority = c(3, 1, 4, 2, 5),
    due = as.Date(c("2024-03-15", "2024-04-09", "2025-03-16", "2023-12-31", "2022-07-20"))
  )

  # Test sorting by date in ascending order
  sorted_other_date_asc <- sort_items(other_object, date = TRUE, ascending = TRUE)
  expect_true(all(sorted_other_date_asc$due == sort(other_object$due)))

  # Test sorting by date in descending order
  sorted_other_date_desc <- sort_items(other_object, date = TRUE, ascending = FALSE)
  expect_true(all(sorted_other_date_desc$due == sort(other_object$due, decreasing = TRUE)))

  # Test sorting by priority in ascending order
  sorted_other_priority_asc <- sort_items(other_object, date = FALSE, ascending = TRUE)
  expect_true(all(sorted_other_priority_asc$priority == sort(other_object$priority)))

  # Test sorting by priority in descending order
  sorted_other_priority_desc <- sort_items(other_object, date = FALSE, ascending = FALSE)
  expect_true(all(sorted_other_priority_desc$priority == sort(other_object$priority, decreasing = TRUE)))
})
