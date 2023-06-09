% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/moodle_html.R
\name{moodle_html_from_md}
\alias{moodle_html_from_md}
\title{Convert a Markdown file into an HTML block to be copied and
pasted for Moodle.}
\usage{
moodle_html_from_md(
  file,
  dir = dirname(file),
  stylesheet = getOption("omuecon.stylesheet", "style.css"),
  template = NULL,
  debug = FALSE,
  ...
)
}
\arguments{
\item{file}{File path to a markdown document.}

\item{dir}{Path to the output directory.}

\item{stylesheet}{Path to a CSS file.}

\item{template}{Path to an HTML template.}

\item{debug}{logical. If set to True, intermediate md file is kept.}

\item{...}{parameters passed on to \code{\link[=moodle_html_from_html]{moodle_html_from_html()}}}
}
\value{
character vector representing the resulting document (HTML fragment).
}
\description{
Convert a Markdown file into an HTML block to be copied and
pasted for Moodle.
}
