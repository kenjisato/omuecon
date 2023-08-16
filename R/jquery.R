
jquery <- function(){

  jq <- if (getd(the$jquery, FALSE)) {
    NULL
  } else {
    the$jquery <- TRUE
    '<script
  src="https://code.jquery.com/jquery-3.7.0.slim.min.js"
  integrity="sha256-tG5mcZUtJsZvyKAxYLVXrmjKBVLd6VpVccqz/r4ypFE="
  crossorigin="anonymous"></script>'
  }
  jq
}

jquery_modal <- function() {
  jq <- jquery()
  jqm <- if (getd(the$jquery_modal, FALSE)) {
    NULL
  } else {
    the$jquery_modal <- TRUE
    c(
      jq,
      '<script src="https://cdnjs.cloudflare.com/ajax/libs/jquery-modal/0.9.1/jquery.modal.min.js"></script>',
      '<script src="https://kenjisato.github.io/omuecon/inst/js/modal.js"></script>'
    )
  }
  paste(jqm, collapse = "\n")
}
