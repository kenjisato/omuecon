with_ext <- function(x, ext, dir = NULL) {
  new_path <- paste0(tools::file_path_sans_ext(x), ".", ext)
  if (is.null(dir)) {
    new_path
  } else {
    file.path(dir, basename(new_path))
  }
}

weekdays_ja <- function(d) {
  dplyr::case_match(
    weekdays(d),
    "Sunday" ~ "\u65e5",
    "Monday" ~ "\u6708",
    "Tuesday" ~ "\u706b",
    "Wednesday" ~ "\u6c34",
    "Thursday" ~ "\u6728",
    "Friday" ~ "\u91d1",
    "Saturday" ~ "\u571f"
  )
}

formatDate <- function(dates, year = FALSE, weekday = TRUE) {
  fmt <- "{lubridate::month(dates)}/{lubridate::day(dates)}"
  if (year) fmt <- paste0("{lubridate::year(dates)}/", fmt)

  wd <- function(x)
    if (weekday) paste0(" (", weekdays_ja(as.Date(dates)), ")") else  ""

  dplyr::if_else(
    is.na(dates),
    NA,
    paste0(stringr::str_glue(fmt), wd(dates))
  )
}


pkg_file <- function(..., pkg = .packageName) {
  system.file(..., package = pkg, mustWork = TRUE)
}


getd <- function(x, default = NULL) {
  if (!is.null(x)) {
    x
  } else {
    default
  }
}


dodge_name <- function(a, b){
  clash <- normalizePath(a, mustWork = FALSE) == normalizePath(b, mustWork = FALSE)
  if (clash && file.exists(b)) {
    paste0(tools::file_path_sans_ext(a), "-out.html")
  } else {
    a
  }
}

