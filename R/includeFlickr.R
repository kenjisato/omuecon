
#' Include Flickr album
#'
#' @param albumUrl character. Share URL of a public album
#' @param title character. Default NULL. If given, overwrite the title of the album.
#' @param width integer. Width of the widget in pixel.
#' @param height integer. Height of the widget in pixel.
#'
#' @return Flickr embed code.
#' @export
#'
includeFlickr <- function(albumUrl, title = NULL, width = 480, height = 360) {

  html <- rvest::read_html(albumUrl)
  image_url <-
    html %>%
    rvest::html_node('[property="og:image"]') %>%
    rvest::html_attr("content")

  if (is.null(title)) {
    title <-
      html %>%
      rvest::html_node('[property="og:title"]') %>%
      rvest::html_attr("content")
  }

  album_url <-
    html |>
    rvest::html_node('[property="og:url"]') %>%
    rvest::html_attr("content") %>%
    stringr::str_replace("sets", "albums")

  x <- glue::glue('<a data-flickr-embed="true" href="{album_url}" title="{title}"><img src="{image_url}" width="{width}" height="{height}" alt="{title}"/></a><script async src="https://embedr.flickr.com/assets/client-code.js" charset="utf-8"></script>')
  knitr::asis_output(x)
}
