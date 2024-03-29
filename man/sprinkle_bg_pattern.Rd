% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sprinkle_bg_pattern.R
\name{sprinkle_bg_pattern}
\alias{sprinkle_bg_pattern}
\alias{sprinkle_bg_pattern.default}
\alias{sprinkle_bg_pattern.dust_list}
\title{Row and Column Background Striping}
\usage{
sprinkle_bg_pattern(
  x,
  rows = NULL,
  cols = NULL,
  bg_pattern = c("transparent", "#DCDCDC"),
  bg_pattern_by = c("rows", "cols"),
  ...,
  part = c("body", "head", "foot", "interoot", "table")
)

\method{sprinkle_bg_pattern}{default}(
  x,
  rows = NULL,
  cols = NULL,
  bg_pattern = c("transparent", "#DCDCDC"),
  bg_pattern_by = c("rows", "cols"),
  ...,
  part = c("body", "head", "foot", "interfoot", "table")
)

\method{sprinkle_bg_pattern}{dust_list}(
  x,
  rows = NULL,
  cols = NULL,
  bg_pattern = c("transparent", "#DCDCDC"),
  bg_pattern_by = c("rows", "cols"),
  ...,
  part = c("body", "head", "foot", "interfoot", "table")
)
}
\arguments{
\item{x}{An object of class \code{dust}}

\item{rows}{Either a numeric vector of rows in the tabular object to be 
modified or an object of class \code{call}.  When a \code{call}, 
generated by \code{quote(expression)}, the expression resolves to 
a logical vector the same length as the number of rows in the table.
Sprinkles are applied to where the expression resolves to \code{TRUE}.}

\item{cols}{Either a numeric vector of columns in the tabular object to
be modified, or a character vector of column names. A mixture of 
character and numeric indices is permissible.}

\item{bg_pattern}{A character vector giving the colors to be iterated in 
the pattern.}

\item{bg_pattern_by}{A subset of \code{c("rows", "cols")}, with partial
matching accepted.  Only the first value is used, and determines the 
direction of the pattern.}

\item{...}{Additional arguments to pass to other methods. Currently ignored.}

\item{part}{A character string denoting which part of the table to modify.}
}
\description{
Provides background color striping based on row or column.
  Striping may be done with any number of colors. The most common use of 
  striping is to provide row discrimination in tables.
}
\section{Functional Requirements}{

\enumerate{
  \item Correctly reassigns the appropriate elements \code{bg} column
   in the table part.
 \item Casts an error if \code{x} is not a \code{dust} object.
 \item Casts an error if \code{bg_pattern} is not a character vector.
 \item Casts an error if any element in \code{bg_pattern} is not a valid
   color name.
 \item Casts an error if \code{bg_pattern_by} is not a subset of 
   \code{c("rows", "columns")} (with partial matching).
 \item Casts an error if \code{part} is not one of \code{"body"}, 
   \code{"head"}, \code{"foot"}, or \code{"interfoot"}
}

This is a rare sprinkle that doesn't use the \code{fixed} and \code{recycle}
arguments.  They are assumed to be \code{FALSE} and \code{"none"}, 
respectively, in order to pass through \code{index_to_sprinkle}.
}

\seealso{
\code{\link{sprinkle_bg}}, \code{\link{sprinkle}}, 
  \code{\link{index_to_sprinkle}}
}
