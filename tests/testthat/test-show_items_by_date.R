library(testthat)
library(taskPlanner)

# Load and create todo list
todo_list_data <- import_todo_list()
my_todo_list <- create_todo_list(todo_list_data)

# Tests for show_items_by_date function
test_that("show_items_by_date handles missing or invalid date input", {
  # Test for missing date input
  expect_error(show_items_by_date(my_todo_list), "Please provide a date in the format 'YYYY-MM-DD'.")

  # Test for invalid date format
  expect_error(show_items_by_date(my_todo_list, date = "invalid-date-format"), "Please provide a valid date in the format 'YYYY-MM-DD'.")
})


# Tests for show_items_by_date.default method
test_that("show_items_by_date.default handles missing or invalid date input", {

  # Create a sample object for testing default method
  other_object <- list(
    task = paste("Task", 1:5),
    priority = c(3, 1, 4, 2, 5),
    due = as.Date(c("2024-03-15", "2024-04-09", "2025-03-16", "2023-12-31", "2022-07-20"))
  )

  # Test for missing date input
  expect_error(show_items_by_date(other_object), "Please provide a date in the format 'YYYY-MM-DD'.")

  # Test for invalid date format
  expect_error(show_items_by_date(other_object, date = "invalid-date-format"), "Please provide a valid date in the format 'YYYY-MM-DD'.")
})

# Test for no items matching the filter criteria
test_that("show_items_by_date prints message for no matching items", {
  # Create a TodoList with no items
  empty_todo_list <- create_todo_list(data.frame(task = character(0), priority = integer(0), due = as.Date(character(0))))

  # Test the function with an empty TodoList
  expect_output(show_items_by_date(empty_todo_list, date = "2024-01-01"), "No items matching the filter criteria.")
})
