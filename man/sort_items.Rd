% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sort_items.R
\name{sort_items}
\alias{sort_items}
\title{Sort Items by Date or Priority}
\usage{
sort_items(x, date = TRUE, ascending = TRUE, ...)
}
\arguments{
\item{x}{An object of class 'TodoList' or other applicable classes.}

\item{date}{Logical, indicating whether to sort by date. If TRUE, sorting by date; if FALSE, sorting by priority.}

\item{ascending}{Logical, indicating the sorting order. If TRUE, ascending order; if FALSE, descending order.}

\item{...}{additional arguments}
}
\value{
The sorted object.
}
\description{
This function sorts items based on specified criteria such as date or priority.
}
\examples{

# Sorting a TodoList by date in ascending order

list <- import_todo_list()
my_todo_list <- create_todo_list(list)

sort_items(my_todo_list, date = TRUE, ascending = TRUE)

# Sorting a TodoList by priority in descending order
list <- import_todo_list()
my_todo_list <- create_todo_list(list)
sort_items(my_todo_list, date = FALSE, ascending = FALSE)

}
