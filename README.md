
<!-- README.md is generated from README.Rmd. Please edit that file -->

# taskPlanner <img src="man/figures/logo.png" align="right" height="100" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/numbats/assignment-1-package-creation-agoh0008/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/numbats/assignment-1-package-creation-agoh0008/actions/workflows/R-CMD-check.yaml)
[![R-CMD-check](https://github.com/Advanced-R-Programming/assignment-3-package-completion-agoh0008/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Advanced-R-Programming/assignment-3-package-completion-agoh0008/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The taskPlanner package aims to facilitate task management by offering a
robust system for organizing to-do lists. With tools for adding,
removing, and updating items, filtering tasks by date and priority, as
well as real-time task tracking, users can leverage these features to
optimize workflow and productivity.

This repository contains the work and materials for Assignments 1 and 3 of the ETC5450 unit (Advanced R Programming).

## Installation

You can install this package directly from [GitHub](https://github.com/)
with:

``` r
# install.packages("devtools")
devtools::install_github("numbats/assignment-1-package-creation-agoh0008")
```

## Functions

- **add_task:** Add a new item into current to-do list by specifying the
  task description, priority and due date.

- **remove_task:** Remove a specific item from current to-do list.

- **update_task:** Update the task description, priority, or due date of
  an existing item in current to-do list.

- **import_todo_list:** Import current to-do list from the “data”
  directory of the package. The to-do list will be empty if no tasks
  were previously added.

- **export_todo_list:** Export the updated to-do list back to a CSV file
  in the “data” directory of the package.

- **create_todo_list():** Create an S3 object of class “TodoList”
  containing all entries from the provided to-do list.

- **show_items_by_date():** Filter items from a to-do list or other
  applicable classes based on a specified date filter criteria.

- **show_items_by_priority():** Filter items from a to-do list or other
  applicable classes based on a specified priority filter criteria.

- **show_today():** Display items due today or earlier from a to-do
  list, along with a bar plot showing the priority of tasks.

- **sort_items():** Sort items in ascending or descending order from a
  to-do list, based on specified criteria such as date or priority.

- **print.TodoList:** Print the tasks in the provided TodoList object
  with their priorities and due dates.

- **plot.TodoList:** Plot the tasks in the provided TodoList object,
  visualizing their priorities using a bar plot.

## Usage Examples

Here are some examples of how to use the functions in the taskPlanner
package:

- Import the current to-do list

``` r
library(taskPlanner)

list <- import_todo_list()
```

- Add new tasks into current to-do list

``` r
list <- add_task(list, "Buy bread", 3, "2024-03-15")
```

- Update task description, priority, or due date of an existing item in
  current to-do list.

``` r
list <- update_task(1, list, "Buy milk", 1, "2024-03-18")
```

- Remove tasks from current to-do list

``` r
list <- remove_task(1, list)
```

- Export the updated to-do list

``` r
export_todo_list(list)
```

- Create an S3 object of class “TodoList”

``` r
my_todo_list <- create_todo_list(list)
```

- Filter items from a to-do list by date

``` r
show_items_by_date(my_todo_list, date = "2024-03-16")
```

- Filter items from a to-do list by priority

``` r
show_items_by_priority(my_todo_list, priority = 2)
```

- Show items due today or earlier (for real-time task tracking)

``` r
show_today(my_todo_list)
```

- Sort items from a to-do list by date or priority

``` r
sort_items(my_todo_list, date = TRUE, ascending = TRUE)
```

- Print items from a to-do list of class ‘TodoList’

``` r
print(my_todo_list)
```

- Plot items from a to-do list of class ‘TodoList’

``` r
plot(my_todo_list)
```

## License

This package is licensed under the GPL License. See the LICENSE file for
details.
