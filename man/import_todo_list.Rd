% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_todo_list.R
\name{import_todo_list}
\alias{import_todo_list}
\title{Import To-Do List}
\usage{
import_todo_list()
}
\value{
The to-do list data frame.
}
\description{
This function imports the to-do list from the "inst" directory of the package.

If the to-do list file does not exist, it creates the file and initializes it with headers.
}
\examples{
list <- import_todo_list()

}
