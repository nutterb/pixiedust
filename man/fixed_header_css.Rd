% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fixed_header_css.R
\name{fixed_header_css}
\alias{fixed_header_css}
\title{Generate CSS Code for Fixed Header Tables}
\source{
Jonas Schubert Erlandsson. https://jsfiddle.net/dPixie/byB9d/3/
}
\usage{
fixed_header_css(
  fixed_header_class_name = "pixie-fixed",
  scroll_body_height = 300,
  scroll_body_height_units = "px",
  scroll_body_background_color = "white",
  fixed_header_height = 20,
  fixed_header_height_units = "px",
  fixed_header_text_height = fixed_header_height/2,
  fixed_header_text_height_units = "px",
  fixed_header_background_color = "white",
  pretty = TRUE
)
}
\arguments{
\item{fixed_header_class_name}{\code{character(1)}. When 
\code{include_fixed_header_css = FALSE}, this
class name is used to reference CSS classes provided by the user to
format the table correctly.}

\item{scroll_body_height}{\code{integerish(1)}. Sets the height of the scrollable
table body.}

\item{scroll_body_height_units}{\code{character(1)}. Determines the units for the
height of the scrollable table.  Defaults to \code{"px"}.  Must be one
of \code{c("px", "pt", "\%", "em")}.}

\item{scroll_body_background_color}{\code{character(1)}. The color of the background
of the body.  Must be a valid color.  It defaults to white, which may
override CSS settings provided by the user.  If this needs to be avoided,
you may use the \code{\link{fixed_header_css}} function to assist in
generating CSS code to use to define the CSS. See Avoiding CSS Conflicts.}

\item{fixed_header_height}{\code{integerish(1)}. Sets the height of the header
row.}

\item{fixed_header_height_units}{\code{character(1)}. Determines the units for the
height of the header row. Defaults to \code{"px"}. Must be one of
\code{c("px", "pt", "\%", "em")}.}

\item{fixed_header_text_height}{\code{numeric(1)}. Sets the height at which the
header text appears.  By default it is set to half of the header height.
This should be approximately centered, but you may alter this to get the
precise look you want.}

\item{fixed_header_text_height_units}{\code{character(1)}. Determines the units for
placing the header text.  Defaults to \code{"px"}. Must be one of
\code{c("px", "pt", "\%", "em")}.}

\item{fixed_header_background_color}{\code{character(1)}. Sets the background color for
the header row.  This defaults to white and may override the user's CSS
settings.  See Avoiding CSS Conflicts.}

\item{pretty}{\code{logical(1)}. When \code{TRUE}, the result is printed
to the console using \code{cat}, making it easy to copy and paste the
code to another document.  When \code{FALSE}, it is returned as a
character string.}
}
\description{
Tables with a fixed header may be generated to permit the 
  headings to remain visible with the data.  The CSS is not difficult, 
  but it not-trivial and requires some coordination across a few 
  parts.  This functions standardizes the generation of the CSS code 
  using as few elements as possible.  Note that there is potential for
  conflicts with existing CSS in this method.
}
\details{
CSS doesn't make this kind of table natural.  The solution to 
  generate the fixed headers used by \code{pixiedust} is probably not the 
  best solution in terms of CSS design.  It is, however, the most conducive 
  to generating dynamically on the fly. 
  
  The fixed header table requires nesting several HTML elements. 
  \enumerate{
   \item a \code{div} tag is used to control the alignment of the table
   \item a \code{section} tag is used to set up the header row that remains fixed.
   \item a \code{div} that sets the height of the scrollable body
   \item the \code{table} tag establishes the actual table.
   \item The \code{th} tags inside the table are set to full transparency and
     the content of the headers is duplicated in a \code{div} within the 
     \code{th} tag to display the content.
  }
  
  To accomplish these tasks, some CSS is exported with the table and placed
  in the document immediately before the table.  Read further to understand
  the conflicts that may arise if you are using custom CSS specifications 
  in your documents.
}
\section{Avoiding CSS Conflicts}{
 
Because of all of the shenanigans involved, exporting the CSS with the tables
may result in conflicts with your custom CSS. Most importantly, any CSS
you have applied to the \code{th} or \code{td} tags may be overwritten.
If you are using custom CSS, you may want to consider using 
\code{include_fixed_header_css = FALSE} and then utilizing 
\code{\link{fixed_header_css}} to generate CSS you can include in your 
CSS file to provide the fixed headers.  The code generated by 
\code{fixed_header_css} ought to be placed before your definitions for
\code{td} and \code{th}.  

To get the same header design in the fixed table, you will want to modify 
the \code{.th-pixie-fixed div} definition in the CSS to match your desired
\code{th} definition.

The code produced by \code{fixed_header_css} will include comments where
there is potential for a CSS conflict.
}

\section{Functional Requirements}{

\enumerate{
 \item If \code{pretty = TRUE} print results to the console.
 \item If \code{pretty = FALSE} Return a character string of length 1.
 \item Cast an error if \code{scroll_body_height} is not \code{integerish(1)}
 \item Cast an error if \code{scroll_body_height_units} is not \code{character(1)}
 \item Cast an error if \code{scroll_body_background_color} is not \code{character(1)}
 \item Cast an error if \code{scroll_body_background_color} is not a valid color.
 \item Cast an error if \code{fixed_header_height} is not \code{integerish(1)}
 \item Cast an error if \code{fixed_header_height_units} is not \code{character(1)}
 \item Cast an error if \code{fixed_header_text_height} is not \code{numeric(1)}
 \item Cast an error if \code{fixed_header_text_height_units} is not \code{character(1)}
 \item Cast an error if \code{fixed_header_background_color} is not \code{character(1)}
 \item Cast an error if \code{fixed_header_background_color} is not a valid color.
 \item Cast an error if \code{pretty} is not \code{logical(1)}
}
}

