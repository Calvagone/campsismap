% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shiny_target_table_editor.R
\name{TargetTableEditor}
\alias{TargetTableEditor}
\title{Target table editor.}
\usage{
TargetTableEditor(
  tableReact = NULL,
  ns = shiny::NS("target_table"),
  fun = NULL,
  initialTarget = 100,
  initialTime = "00:00",
  greyOutPast = FALSE,
  dateOnly = TRUE
)
}
\arguments{
\item{tableReact}{reactive table}

\item{ns}{shiny namespace}

\item{fun}{conversion function (table -> anything)}

\item{initialTarget}{first target value in table, only if tableReact is NULL}

\item{initialTime}{first target time in table, only if tableReact is NULL}

\item{greyOutPast}{grey out the rows in past, logical value}

\item{dateOnly}{show only date in editor, logical value}
}
\description{
Target table editor.
}
