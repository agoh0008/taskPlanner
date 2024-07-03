library(testthat)
library(taskPlanner)

test_that("import_todo_list imports to-do list from inst directory", {

  sample_todo_list <- data.frame(
    task = c("Do assignments", "Buy cake"),
    priority = c(3L, 4L),
    due = as.Date(c("2024-03-15", "2024-03-16"), format = "%Y-%m-%d"),
    stringsAsFactors = FALSE
  )

  # Export the sample to-do list to a CSV file
  write.csv(sample_todo_list, file = system.file("todo_list.csv", package = "taskPlanner"),
            row.names = FALSE)

  imported_todo_list <- import_todo_list()

  # Check if the imported to-do list matches the sample to-do list
  expect_identical(imported_todo_list, sample_todo_list,
                   "Imported to-do list should match the sample to-do list")
})
