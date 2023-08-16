#' Convert a HTML file into a HTML block to be copied and pasted for Moodle.
#'
#' @param in_text character. Character vector representing the HTML file to convert.
#' @param full_html logical. If TRUE, produce complete html output,
#'   convenient when drafting a document. When this option is enabled, tag and
#'   id and remove_script options are silently ignored.
#' @param tag character. Outer-most tag for the resulting HTML snippet.
#' @param id character. id attribute for the outer-most tag.
#' @param remove_script logical. If TRUE, script tags are all stripped out.
#'
#' @return character. HTML block.
convert_html2moodle <- function(
    in_text,
    full_html = getd(the$full_html, FALSE),
    tag = getd(the$tag, "body"), id = the$id,
    remove_script = getd(the$remove_script, FALSE)) {
  # Conversion
  inlined_html <- css_inline(in_text)
  container_tag <- if (full_html) {
    inlined_html
  } else {
    extract_tag(inlined_html, tag, id, remove_script)
  }
  strsplit(as.character(container_tag), "\n")[[1]]
}


css_inline <- function(html_text) {
  orig_html <- paste(html_text, collapse = "\n")
  inlined_html <-
    orig_html %>%
    juicyjuice::css_inline() %>%
    rvest::read_html()
  inlined_html
}


extract_tag <- function(html, tag = "body", id = NULL, remove_script = FALSE) {
  body <- rvest::html_element(html, "body")

  if (remove_script) {
    scripts <- rvest::html_elements(body, "script")
    for (script in scripts) {
      xml2::xml_remove(script)
    }
  }

  body_attr <- rvest::html_attrs(body)
  body_children <- rvest::html_children(body)

  container_tag <-
    rvest::read_html(str_glue("<{tag}></{tag}>")) %>%
    rvest::html_element(tag)

  if (!is.null(id)) xml2::xml_attr(container_tag, "id") <- id
  if (length(body_attr) > 0) xml2::xml_attr(container_tag, "style") <- body_attr

  for (child in body_children) {
    xml2::xml_add_child(container_tag, child)
  }

  container_tag
}
