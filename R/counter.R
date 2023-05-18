
#' Counter factory.
#'
#' @param label character. Default prefix for the counter.
#' @param open character. Opening bracket such as "(" or "["
#' @param close character. Closing bracket such as ")" or "]"
#' @param sep character. Separator between label and counter.
#' @param link logical. If TRUE, get() method returns a counter
#'   surrounded by a tag, linked to the referenced id.
#'
#' @return List of functions to set and get counters with label.
#' @export
#'
#' @examples
#' cnt <- counter("Quiz")
#' cnt$set("aaa")
#' cnt$set()
#' cnt$set("bbb", quiet = TRUE)
#' cnt$get("aaa")
#' cnt$get("bbb")
#' cnt$get("aaa", type = "bare")
#' cnt$dump()
counter <- function(label, open = NULL, close = NULL, sep = " ", link = FALSE) {
  n <- 0L
  record <- list()

  span <- function(n) {
    if (link)
      paste0("<span id='", label, "-", n,"'></span>")
    else
      NULL
  }

  atag <- function(n) {
    if (link)
      paste0("<a href='#", label, "-", n,"'>", n, "</a>")
    else
      as.character(n)
  }

  the <- function(n, type = c("full", "bare", "num")){
    type <- match.arg(type)
    num <- atag(n)
    switch (type,
      "full" = paste(label, paste0(open, num, close), sep = sep),
      "bare" = paste0(open, num, close),
      "num" = num
    )
  }

  list(
    get = function(id = NULL, type = c("full", "bare", "num")) {
      if (!is.null(id) && is.null(record[[id]])) {
        # missing record
        return(the("??", type))
      }

      if (is.null(id)) {
        # get the latest record if id is NULL.
        num <- as.character(length(record))
      } else {
        num <- as.character(record[[id]])
      }
      paste0(the(num, type), span(num))
    },
    set = function(id = NULL, quiet = FALSE, type = c("full", "bare", "num")) {
      type <- match.arg(type)
      if (is.numeric(id)) stop("id must be a character string.")
      if (!is.null(id) && id %in% names(record))
        stop(paste("id", sQuote(id), "already in use."))
      n <<- n + 1

      if (is.null(id)) {
        record[[n]] <<- n
      } else {
        record[[id]] <<- n
      }
      if (quiet)
        invisible(as.character(n))
      else
        the(n, type)
    },
    dump = function() {
      record
    }
  )
}


