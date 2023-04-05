
css_resolve <- function(css) {

  if (file.exists(css)) {
    css <- readLines(css)
  }

  if (length(css) > 1) {
    css <- paste(css, collapse = "\n")
  }

  variables <-
    css |>
    str_extract(
      pattern = stringr::regex(":root\\s*\\{([^\\}]*)\\}", dotall = TRUE),
      group = 1
    ) |>
    str_remove(";$") |>
    str_split_1(";") |>
    str_split(":", simplify = TRUE)

  keys <- str_squish(variables[, 1])
  vals <- str_squish(variables[, 2])
  names(vals) <- str_glue("var\\({keys}\\)")

  str_replace_all(css, vals)
}





