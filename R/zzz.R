.onLoad <- function(libname, pkgname) {
  op <- options()
  op.omuecon <- list(
    omuecon.template = pkg_file("xml", "template.html"),
    omuecon.article.css = pkg_file("css", "style.css"),
    omuecon.div.css = pkg_file("css", "label.css")
  )
  toset <- !(names(op.omuecon) %in% names(op))
  if (any(toset)) options(op.omuecon[toset])

  invisible()
}

.onAttach <- function(libname, pkgname) {
  # packageStartupMessage("Welcome to omuecon")
}
