
modal_dict_verify <- function(dict) {
  # verify columns
  columns <- c("id", "term", "definition")
  missing_col <- is.na(match(columns, names(dict)))
  if (any(missing_col)) {
    message(glue::glue("Column `{columns[missing_col]}` is missing.\n\n"))
    stop("Incomplete dictionary. Abort.")
  }

  # verify rows
  if (any(duplicated(dict$id))) {
    stop(glue::glue("Duplicated id: {paste(dict$id[duplicated(dict$id)])}"))
  }
  if (any(duplicated(dict$term))) {
    stop(glue::glue("Duplicated term: {paste(dict$term[duplicated(dict$term)])}"))
  }

  # id as character
  dict$id <- as.character(dict$id)

  dict
}


modal_register <- function(dict = NULL) {

  if(!getd(the$modal, FALSE)) {
    stop("modal_setup() has not been invoked.")
  }
  prefix <- the$modal_prefix

  if (!is.null(dict)) {
    dict <- modal_dict_verify(dict)
    the$modal_dict <- dict
  }

  register <- function(term, id = NULL) {

    if (!is.null(dict)) {
      .id <- dict$id[dict$term == term]
      if (!is.null(id) && id != .id)
        stop(glue::glue("conflicting id for {term}."))
      id <- .id
      the$modal_entries[[id]] <- term
    } else {
      if (is.null(id))
        stop(glue::glue("missing id for {term}."))
    }
    knitr::asis_output(glue::glue('<a href="#{prefix}{id}" rel="modal:open" class="modal-open">{term}</a>'))
  }

  register
}

modal_setup <- function(prefix = "modal-") {

  the$modal <- TRUE
  the$modal_prefix <- prefix
  the$modal_entries <- new.env(parent = emptyenv())

  jqm <- jquery_modal()

  css <- glue::glue("<style>
  a.modal-open {
    color: black;
    text-decoration: none;
    border-bottom: 1px black dashed;
    cursor: pointer;
  }
  .modal h2 {
    font-size: 100%;
    border: none;
    border-bottom: 1px dashed gray;
    background-color: white;
    text-align: left;
    padding-bottom: 0;
    padding-left: 10px;
    margin-bottom: 5px;
  }
  </style>", .open = "(", .close = ")")

  x <- paste(css, jqm, sep = "\n")
  knitr::asis_output(x)
}


modal_body <- function(dict = NULL) {

  if(is.null(the$modal)) {
    stop("modal_setup() has not been invoked.")
  }

  if(!is.null(the$modal_dict)) {
    dict <- the$modal_dict
  } else {
    if (is.null(dict)) {
      stop("First positional argument, dict, is not given.")
    }
    dict <- modal_dict_verify(dict)
  }

  templ <- "
::: {#((the$modal_prefix))((id)) .modal}
## ((term))

((definition))
:::
"
  dict[dict$id %in% ls(the$modal_entries), ] %>%
    glue::glue_data(templ, .open = "((", .close = "))") %>%
    knitr::asis_output()
}
