library(testthat)
library(taskPlanner)

# Test if the function creates a TodoList object from a provided data frame
test_that("create_todo_list creates TodoList from provided data frame", {
  # Create a sample data frame
  sample_data <- data.frame(
    task = c("Task1", "Task2"),
    priority = c(1, 2),
    due = as.Date(c("2024-01-01", "2024-01-02")),
    stringsAsFactors = FALSE
  )

  # Call the function with the sample data frame
  todo_list <- create_todo_list(sample_data)

  # Check if the returned object is of class TodoList
  expect_true("TodoList" %in% class(todo_list))
})

# Test if the function imports the current to-do list if no data frame is provided
test_that("create_todo_list imports current to-do list if no data frame provided", {
  # Call the function without providing a data frame
  todo_list <- create_todo_list()

  # Check if the returned object is of class TodoList
  expect_true("TodoList" %in% class(todo_list))
})


# Test for incorrect to-do list format

test_that("create_todo_list doesn't work for data frames with incorrect columns", {
  # Generate a random dataframe for testing (not a todo list)
  set.seed(456)

  # Create random data
  num_rows <- 5
  random_data <- data.frame(
    ID = 1:num_rows,
    Name = replicate(num_rows, paste(sample(LETTERS, size = 5), collapse = "")),
    Value = rnorm(num_rows)
  )

  expect_error(create_todo_list(random_data), "Input dataframe must have columns: task, priority, and due.")
})


# Test for incorrect to-do list format

test_that("create_todo_list doesn't work for data frames with incorrect formats", {

  sample_todo_list <- data.frame(
    task = c("Do assignments", "Buy cake"),
    priority = c("Low", "High"),
    due = as.Date(c("2024-03-15", "2024-03-16"), format = "%Y-%m-%d")
  )

  expect_error(create_todo_list(sample_todo_list),
               "Columns 'task' must be in 'character' format, 'priority' must be in 'numeric' format, and 'due' must be in 'Date' format.")
})


