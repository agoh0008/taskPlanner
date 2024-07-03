library(testthat)
library(taskPlanner)

test_that("export_todo_list exports updated to-do list to CSV file in inst directory", {

  sample_todo_list <- data.frame(
    task = c("Do assignments", "Buy cake"),
    priority = c(3L, 4L),
    due = c("2024-03-15", "2024-03-16"),
    stringsAsFactors = FALSE
  )

  export_todo_list(sample_todo_list)

  # Test case 1: check if the CSV file is created in the inst directory
  todo_list_file <- system.file("todo_list.csv", package = "taskPlanner")

  expect_true(file.exists(todo_list_file), "CSV file should be created in the inst directory")

  exported_todo_list <- read.csv(todo_list_file, stringsAsFactors = FALSE)

  # Test case 2: check if the exported to-do list matches the sample to-do list
  expect_identical(exported_todo_list, sample_todo_list,
                   "Exported to-do list should match the sample to-do list")
})
