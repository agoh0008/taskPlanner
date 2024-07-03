library(testthat)
library(taskPlanner)

# Load or create your todo list
todo_list_data <- import_todo_list()
my_todo_list <- create_todo_list(todo_list_data)

# Tests for show_items_by_priority function
test_that("show_items_by_priority handles missing or invalid priority input", {
  # Test for missing priority input
  expect_error(show_items_by_priority(my_todo_list), "Please provide a priority number between 1 to 5.")

  # Test for invalid priority format
  expect_error(show_items_by_priority(my_todo_list, priority = "invalid-priority"), "Priority must be an integer between 1 and 5.")
})


# Tests for show_items_by_priority.default method
test_that("show_items_by_priority.default handles missing or invalid priority input", {

  # Create a sample object for testing default method
  other_object <- list(
    task = paste("Task", 1:5),
    priority = c(3, 1, 4, 2, 5),
    due = as.Date(c("2024-03-15", "2024-04-09", "2025-03-16", "2023-12-31", "2022-07-20"))
  )

  # Test for missing priority input
  expect_error(show_items_by_priority(other_object), "Please provide a priority number between 1 to 5.")

  # Test for invalid priority format
  expect_error(show_items_by_priority(other_object, priority = "invalid-priority"), "Priority must be an integer between 1 and 5.")
})

# Test for no items matching the filter criteria
test_that("show_items_by_priority.default prints message for no matching items", {
  # Create a TodoList with no items
  empty_todo_list <- create_todo_list(data.frame(task = character(0), priority = integer(0), due = as.Date(character(0))))

  # Test the function with an empty TodoList
  expect_output(show_items_by_priority(empty_todo_list, priority = 2), "No items matching the filter criteria.")
})
