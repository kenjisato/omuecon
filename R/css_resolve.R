
css_resolve <- function(css) {

  if (file.exists(css)) {
    css <- readLines(css)
  }

  if (length(css) > 1) {
    css <- paste(css, collapse = "\n")
  }

  variables <-
    css %>%
    str_extract(
      pattern = stringr::regex(":root\\s*\\{([^\\}]*)\\}", dotall = TRUE),
      group = 1
    ) %>%
    str_remove(";$")

  if (is.na(variables)) {
    return(css)
  }

  variables <-
    variables %>%
    str_split_1(";") %>%
    str_split(":", simplify = TRUE)

  keys <- str_squish(variables[, 1])
  vals <- str_squish(variables[, 2])
  names(vals) <- str_glue("var\\({keys}\\)")

  str_replace_all(css, vals)
}


css_find <- function(stylesheets) {
  user_file <- file.exists(stylesheets)

  if (any(!user_file)) {
    system_file <- system.file("css", stylesheets[!user_file],
                               package = .packageName, mustWork = TRUE)
    stylesheets[!user_file] <- system_file
  }
  stylesheets
}



