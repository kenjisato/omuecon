
#' Counter factory.
#'
#' @param label character. Default prefix for the counter.
#' @param sep character. Separator between label and counter.
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
#' cnt$get("aaa", bare = TRUE)
#' cnt$dump()
counter <- function(label, sep = " ") {
  n <- 0L
  record <- list()

  list(
    get = function(id = NULL, bare = FALSE) {
      if (is.null(id)) {
        # get the latest record if id is NULL.
        num <- as.character(length(record))
      } else if (is.null(record[[id]])) {
        num <- "??"
      } else {
        num <- as.character(record[[id]])
      }
      if (bare) {
        num
      } else {
        paste(label, num, sep = sep)
      }
    },
    set = function(id = NULL, quiet = FALSE) {
      if (is.numeric(id)) stop("id must be a character string.")
      if (!is.null(id) && id %in% names(record)) stop(paste("id", sQuote(id), "already in use."))
      n <<- n + 1

      if (is.null(id)) {
        record[[n]] <<- n
      } else {
        record[[id]] <<- n
      }
      if (quiet) invisible(as.character(n)) else paste(label, n, sep = sep)
    },
    dump = function() {
      record
    }
  )
}


